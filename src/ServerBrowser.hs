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
		  ("_Game"	, 0	, False	, False	, False	, showGame , Just (comparing (\x -> (protocol x, gamemod x))))
		, ("_Name"	, 0	, True	, True	, True	, pangoPretty colors . hostname	, Just (comparing hostname))
		, ("_Map"	, 0	, True	, False	, False	, take 16 . unpackorig . mapname	, Just (comparing mapname))
		, ("P_ing"	, 1	, False	, False , False	, show . gameping	, Just (comparing gameping))
		, ("_Players"	, 1	, False	, False , False	, showPlayers		, Just (comparing nplayers))
		]

	(infobox, statNow, statTot, statRequested) <- newInfoboxBrowser
	
	Config {filterBrowser, filterEmpty} <- atomically $ readTMVar mconfig
	(filterbar, current) <- newFilterBar filtered statNow filterBrowser
	empty <- checkButtonNewWithMnemonic "_empty"
	set empty [ toggleButtonActive := filterEmpty ]
	boxPackStart filterbar empty PackNatural 0
	on empty toggled $ do
		treeModelFilterRefilter filtered
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]


	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		GameServer{..}	<- treeModelGetRow rawmodel iter
		s		<- readIORef current
		showEmpty	<- toggleButtonGetActive empty
		return $ (showEmpty || not (null players)) && (B.null s ||
			smartFilter s [
				  cleanedCase hostname
				, cleanedCase mapname
				, proto2string protocol
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
		PollResult{..} <- atomically $ readTMVar mpolled
		listStoreClear rawmodel
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend rawmodel) polled
		treeModelFilterRefilter filtered
		set statTot		[ labelText := show serversResponded ]
		set statRequested	[ labelText := show (serversRequested-serversResponded) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]
		
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateF)
	where
	showGame GameServer{..} = proto2string protocol ++ maybe "" (("-"++) . htmlEscape . unpackorig) gamemod
	showPlayers GameServer{..} = printf "%d / %2d" nplayers slots
