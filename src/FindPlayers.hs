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


newFindPlayers :: Bundle -> SetCurrent -> IO (VBox, PolledHook)
newFindPlayers Bundle{..} setServer = do
	Config {..}	<- atomically $ readTMVar mconfig
	let raw		= playerStore
	filtered	<- treeModelFilterNew raw []
	sorted		<- treeModelSortNewWithModel filtered	
	view		<- treeViewNewWithModel sorted
	
	addColumnsFilterSort raw filtered sorted view playersSort playersOrder
		[ ("Name"	, True	, RendText (simpleColumn colors fst)
			, Just (comparing fst))
		, ("Server"	, True	, RendText (simpleColumn colors (hostname . snd))
			, Just (comparing (hostname .snd))) 
		]

	(infobox, statNow, statTot)	<- newInfobox "players"
	(filterbar, current)	<- newFilterBar parent filtered statNow filterPlayers
	
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
		mapM_ (listStoreAppend raw) (makePlayerNameList polled)
		treeModelFilterRefilter filtered
		labelSetText statTot . show =<< treeModelIterNChildren raw Nothing
		labelSetText statNow . show =<< treeModelIterNChildren filtered Nothing

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
	
	return (box, updateFilter)
	where simpleColumn colors f item =
		[ cellTextEllipsize := EllipsizeEnd
		, cellTextMarkup := Just $ pangoPretty colors (f item) ]


playerUpdateOne :: PlayerStore -> GameServer -> IO ()
playerUpdateOne raw gs = do
	lst <- listStoreToList raw
	let oldplayers = [ i | (i, (_, gs')) <- zip [0::Int ..] lst, address gs == address gs']
	let newplayers = map (\a -> (name a, gs)) (players gs)
	foldPlayers newplayers oldplayers
	where
	foldPlayers (n:ns) (i:os) = listStoreSetValue raw i n >> foldPlayers ns os
	foldPlayers []     (i:os) = listStoreRemove raw i     >> foldPlayers [] os
	foldPlayers (n:ns) []     = listStoreAppend raw n     >> foldPlayers ns []
	foldPlayers []     []     = return ()

	
