module FindPlayers where
import Graphics.UI.Gtk

import Data.IORef
import Data.Ord
import Network.Tremulous.Protocol
import Network.Tremulous.Util
import qualified Network.Tremulous.StrictMaybe as SM

import Types
import GtkUtils
import GtkExts
import TremFormatting
import FilterBar
import InfoBox
import Constants
import Config


newFindPlayers :: Bundle -> SetCurrent -> IO (VBox, PolledHook)
newFindPlayers Bundle{..} setServer = do
	Config {..} <- atomically $ readTMVar mconfig
	gen@(GenFilterSort raw filtered sorted view) <- newGenFilterSort playerStore
	
	addColumnFS gen "_Name" True (Just (comparing fst))
		cellRendererTextNew (simpleColumn colors fst)
	addColumnFS gen "_Server" True (Just (comparing (hostname . snd)))
		cellRendererTextNew (simpleColumn colors (hostname . snd))

	treeSortableSetSortColumnId sorted playersSort playersOrder
	
	(infobox, statNow, statTot)	<- newInfobox "players"
	(filterbar, current)		<- newFilterBar parent filtered statNow filterPlayers
	
	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(item, GameServer{..}) <- treeModelGetRow raw iter
		s <- readIORef current
		return $ smartFilter s
				[ cleanedCase item
				, proto2string protocol
				, SM.maybe "" cleanedCase gamemod
				]
		
	let updateFilter PollResult{..} = do
		listStoreClear raw
		mapM_ (listStoreAppend raw) (makePlayerNameList polled)
		labelSetText statTot . show =<< treeModelIterNChildren raw Nothing
		labelSetText statNow . show =<< treeModelIterNChildren filtered Nothing

	on view cursorChanged $ do
		(path, _) <- treeViewGetCursor view
		setServer False . snd =<< getElementPath gen path

	on view rowActivated $ \path _ -> do
		setServer True . snd =<< getElementPath gen path

	scroll <- scrollIt view PolicyAutomatic PolicyAlways
	
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scroll PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateFilter)
	where simpleColumn colors f rend item = do
		set rend [cellTextEllipsize := EllipsizeEnd]
		cellSetMarkup rend $ pangoPrettyBS colors (f item)


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

	
