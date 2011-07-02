module ServerBrowser where
import Graphics.UI.Gtk
import Control.Concurrent.STM
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Function as F
import Network.Tremulous.Protocol

import GtkUtils
import FilterBar
import InfoBox
import TremFormatting
import Constants
import Config

newServerBrowser :: TMVar Config -> (Bool -> GameServer -> IO ()) -> IO (VBox, [GameServer] -> IO ())
newServerBrowser mconfig setServer = do
	Config {colors} <- atomically $ readTMVar mconfig
	-- Listview (this is a fucking nightmare)
	rawmodel	<- listStoreNew []
	filtered	<- treeModelFilterNew rawmodel []
	model		<- treeModelSortNewWithModel filtered	
	view		<- treeViewNewWithModel model
	
	addColumnsFilterSort rawmodel filtered model view (Just (compare `F.on` gameping)) [
		  ("_Game"	, False	, False	, False	, showGame , Just (compare `F.on` (\x -> (gameproto x, gamemod x))))
		, ("_Name"	, True	, True	, True	, unfuckName colors . hostname	, Just (compare `F.on` hostname))
		, ("_Map"	, True	, False	, False	, take 16 . unpackorig . mapname	, Just (compare `F.on` mapname))
		, ("P_ing"	, False	, False , False	, show . gameping	, Just (compare `F.on` gameping))
		, ("_Players"	, False	, False , False	, showPlayers		, Just (compare `F.on` nplayers))
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
	
	let updateF polled = do
		listStoreClear rawmodel
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend rawmodel) polled
		treeModelFilterRefilter filtered
		set statTot [ labelText := show (length polled) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]
		
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural g_SPACING
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateF)
	where
	showGame x = (proto2string . gameproto) x ++ maybe "" (("-"++) . unpackorig) (gamemod x)
	showPlayers x = (show $ length $ players x) ++ "/" ++  (show $ slots x)	
