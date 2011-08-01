module FindPlayers where
import Graphics.UI.Gtk

import Data.IORef
import Data.Ord
import Network.Tremulous.Protocol
import Network.Tremulous.Util

import Types
import GtkUtils
import TremFormatting
import FilterBar
import InfoBox
import Constants
import Config


newFindPlayers :: Bundle -> SetCurrent -> IO (VBox, PolledHook, Entry)
newFindPlayers Bundle{..} setServer = do
	Config {..}	<- atomically $ readTMVar mconfig
	raw		<- listStoreNew []
	filtered	<- treeModelFilterNew raw []
	sorted		<- treeModelSortNewWithModel filtered	
	view		<- treeViewNewWithModel sorted
	
	addColumnsFilterSort raw filtered sorted view playersSort (if playersOrder then SortDescending else SortAscending)
		[ ("Name"	, True	, RendText (simpleColumn colors fst)
			, Just (comparing fst))
		, ("Server"	, True	, RendText (simpleColumn colors (hostname . snd))
			, Just (comparing (hostname .snd))) 
		]

	(infobox, statNow, statTot)	<- newInfobox "players"
	(filterbar, current, ent)	<- newFilterBar filtered statNow filterPlayers
	
	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(item, GameServer{..}) <- treeModelGetRow raw iter
		s <- readIORef current
		return $ smartFilter s
				[ cleanedCase item
				, proto2string protocol
				, maybe "" cleanedCase gamemod
				]
		
	let updateFilter PollResult{..} = do
		listStoreClear raw
		let plist = makePlayerNameList polled
		mapM_ (listStoreAppend raw) plist
		treeModelFilterRefilter filtered
		set statTot [ labelText := show (length plist) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]
						
	on view cursorChanged $ do
		(path, _) <- treeViewGetCursor view
		setServer False . snd =<< getElementFS raw sorted filtered path

	on view rowActivated $ \path _ -> do
		setServer True . snd =<< getElementFS raw sorted filtered path
	
	scroll <- scrollIt view PolicyAutomatic PolicyAlways
	
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scroll PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateFilter, ent)
	where simpleColumn colors f item =
		[ cellTextEllipsize := EllipsizeEnd
		, cellTextMarkup := Just ( pangoPretty colors (f item)) ]
