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

newServerBrowser :: Bundle -> SetCurrent -> IO (VBox, PolledHook,Entry)
newServerBrowser Bundle{browserStore=raw, ..} setServer = do
	Config {..}	<- atomically $ readTMVar mconfig
	filtered	<- treeModelFilterNew raw []
	sorted		<- treeModelSortNewWithModel filtered	
	view		<- treeViewNewWithModel sorted
	
	addColumnsFilterSort raw filtered sorted view 3 SortAscending
		[ ("_Game"	, False	, RendText (simpleColumn showGame)
		   	, Just (comparing (\x -> (protocol x, gamemod x))))
		, ("_Name"	, True	, RendText (markupColumn colors hostname)
			, Just (comparing hostname))
		, ("_Map"	, False	, RendText (simpleColumn (take 16 . unpackorig . mapname))
			, Just (comparing mapname))
		, ("P_ing"	, False	, RendText (intColumn (show . gameping))
			, Just (comparing gameping))
		, ("_Players"	, False	, RendText (intColumn (showPlayers))
			, Just (comparing nplayers))
		]
	(infobox, statNow, statTot, statRequested) <- newInfoboxBrowser
	
	(filterbar, current, ent) <- newFilterBar filtered statNow filterBrowser
	empty <- checkButtonNewWithMnemonic "_empty"
	set empty [ toggleButtonActive := filterEmpty ]
	boxPackStart filterbar empty PackNatural spacingHalf
	on empty toggled $ do
		treeModelFilterRefilter filtered
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]


	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		GameServer{..}	<- treeModelGetRow raw iter
		s		<- readIORef current
		showEmpty	<- toggleButtonGetActive empty
		return $ (showEmpty || not (null players)) && (B.null s ||
			smartFilter s [
				  cleanedCase hostname
				, cleanedCase mapname
				, proto2string protocol
				, maybe "" cleanedCase gamemod
				])
			
	on view cursorChanged $ do
		(path, _) <- treeViewGetCursor view
		setServer False =<< getElementFS raw sorted filtered path

	on view rowActivated $ \path _ ->
		setServer True =<< getElementFS raw sorted filtered path
		
	scrollview <- scrolledWindowNew Nothing Nothing
	scrolledWindowSetPolicy scrollview PolicyNever PolicyAlways
	containerAdd scrollview view
	
	let updateF PollResult{..} = do
		listStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend raw) polled
		treeModelFilterRefilter filtered
		set statTot		[ labelText := show serversResponded ]
		set statRequested	[ labelText := show (serversRequested-serversResponded) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow 		[ labelText := show n ]
		
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateF, ent)
	where
	showGame GameServer{..} = proto2string protocol ++ maybe "" (("-"++) . htmlEscape . unpackorig) gamemod
	showPlayers GameServer{..} = printf "%d / %2d" nplayers slots
	markupColumn colors f item =
		[ cellTextEllipsize := EllipsizeEnd
		, cellTextMarkup := Just (pangoPretty colors (f item)) ]
	intColumn f item = [ cellText := f item , cellXAlign := 1 ]
	simpleColumn f item = [ cellText := f item ]
