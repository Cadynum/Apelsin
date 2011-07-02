module ServerBrowser where
import Graphics.UI.Gtk
import Control.Concurrent.STM
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Data.Ord
import Network.Tremulous.Protocol
import Text.Printf

import Types
import GtkUtils
import FilterBar
import InfoBox
import TremFormatting
import Constants
import Config

newServerBrowser :: Bundle -> (Bool -> GameServer -> IO ()) -> IO (VBox, IO ())
newServerBrowser Bundle{..} setServer = do
	Config {colors} <- atomically $ readTMVar mconfig
	rawmodel	<- listStoreNew []
	filtered	<- treeModelFilterNew rawmodel []
	model		<- treeModelSortNewWithModel filtered	
	view		<- treeViewNewWithModel model
	
	addColumnsFilterSort rawmodel filtered model view (Just (comparing gameping)) [
		  ("_Game"	, 0	, False	, False	, False	, showGame , Just (comparing (\x -> (gameproto x, gamemod x))))
		, ("_Name"	, 0	, True	, True	, True	, unfuckName colors . hostname	, Just (comparing hostname))
		, ("_Map"	, 0	, True	, False	, False	, take 16 . unpackorig . mapname	, Just (comparing mapname))
		, ("P_ing"	, 1	, False	, False , False	, show . gameping	, Just (comparing gameping))
		, ("_Players"	, 1	, False	, False , False	, showPlayers		, Just (comparing nplayers))
		]

	(infobox, statNow, statTot) <- newInfobox "servers"	
	Config {filterBrowser} <- atomically $ readTMVar mconfig
	(filterbar, current) <- newFilterBar filtered statNow filterBrowser
	empty <- checkButtonNewWithMnemonic "_empty"
	boxPackStart filterbar empty PackNatural 0
	toggleButtonSetActive empty True
	on empty toggled $
		treeModelFilterRefilter filtered


	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		GameServer{..}	<- treeModelGetRow rawmodel iter
		s		<- readIORef current
		showEmpty	<- toggleButtonGetActive empty
		return $ (showEmpty || not (null players)) && (B.null s ||
			smartFilter s [
				  cleanedCase hostname
				, cleanedCase mapname
				, proto2string gameproto
				, maybe "" cleanedCase gamemod
				])
			
	onCursorChanged view $ do
		(x, _)		<- treeViewGetCursor view
		Just vIter	<- treeModelGetIter model x
		sIter		<- treeModelSortConvertIterToChildIter model vIter
		fIter		<- treeModelFilterConvertIterToChildIter filtered sIter
		gameserver	<- treeModelGetRow rawmodel fIter
		setServer False gameserver

	onRowActivated view $ \path _ -> do
		Just vIter	<- treeModelGetIter model path
		sIter		<- treeModelSortConvertIterToChildIter model vIter
		fIter		<- treeModelFilterConvertIterToChildIter filtered sIter
		gameserver	<- treeModelGetRow rawmodel fIter
		setServer True gameserver
		
	
	scrollview <- scrolledWindowNew Nothing Nothing
	scrolledWindowSetPolicy scrollview PolicyNever PolicyAlways
	containerAdd scrollview view
	
	let updateF = do
		polled <- atomically $ readTMVar mpolled
		listStoreClear rawmodel
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend rawmodel) polled
		treeModelFilterRefilter filtered
		set statTot [ labelText := show (length polled) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]
		
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateF)
	where
	showGame GameServer{..} = proto2string gameproto ++ maybe "" (("-"++) . unpackorig) gamemod
	showPlayers GameServer{..} = printf "%d / %2d" nplayers slots
