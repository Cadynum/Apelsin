module ServerBrowser (newServerBrowser, browserUpdateOne) where
import Graphics.UI.Gtk
import Data.IORef
import Data.Ord
import Data.Monoid
import Network.Tremulous.Protocol
import qualified Network.Tremulous.StrictMaybe as SM
import Text.Printf
import qualified Data.ByteString.Char8 as B

import Types
import GtkUtils
import GtkExts
import FilterBar
import InfoBox
import TremFormatting
import Constants
import Config

newServerBrowser :: Bundle -> SetCurrent -> IO (VBox, PolledHook)
newServerBrowser bundle@Bundle{..} setServer = do
	Config {..} <- readMVar mconfig
	gen@(GenFilterSort raw filtered sorted view) <- newGenFilterSort browserStore

	addColumnFS gen "_Game" False
		(Just (comparing protocol `mappend` comparing gamemod `mappend` comparing gameping))
		(rememberColumn bundle 0)
		cellRendererTextNew
		[]
		(\rend -> cellSetText rend . showGame)

	addColumnFS gen "_Name" True
		(Just (comparing hostname `mappend` comparing gameping))
		(rememberColumn bundle 1)
		cellRendererTextNew
		[cellTextEllipsize := EllipsizeEnd]
		(\rend -> cellSetMarkup rend . pangoPrettyBS colors . hostname)

	addColumnFS gen "_Map" False
		(Just (comparing mapname `mappend` comparing gameping))
		(rememberColumn bundle 2)
		cellRendererTextNew []
		(\rend -> cellSetText rend . SM.maybe "" (B.take 16 . original) . mapname)

	addColumnFS gen "P_ing" False
		(Just (comparing gameping `mappend` comparing hostname))
		(rememberColumn bundle 3)
		cellRendererTextNew
		[cellXAlign := 1]
		(\rend x -> set rend [cellText := show $ gameping x])

	addColumnFS gen "_Players" False
		(Just (comparing nplayers `mappend` flip (comparing gameping)))
		(rememberColumn bundle 4)
		cellRendererTextNew
		[cellXAlign := 1]
		(\rend x -> set rend [cellText := showPlayers x])


	treeSortableSetSortColumnId sorted browserSortColumn browserOrder

	(infobox, statNow, statTot, statRequested) <- newInfoboxBrowser

	(filterbar, current) <- newFilterBar parent filtered statNow filterBrowser
	empty <- checkButtonNewWithMnemonic "_empty"
	set empty [ toggleButtonActive := showEmpty ]
	boxPackStart filterbar empty PackNatural spacingHalf
	on empty toggled $ do
		treeModelFilterRefilter filtered
		labelSetText statNow . show =<< treeModelIterNChildren filtered Nothing


	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		GameServer{..}	<- treeModelGetRow raw iter
		s		<- readIORef current
		showEmpty_	<- toggleButtonGetActive empty
		return $ (showEmpty_ || nplayers > 0) &&
			(smartFilter s
				[ cleanedCase hostname
				, SM.maybe "" cleanedCase mapname
				, SM.maybe "" cleanedCase version
				, protoToAbbr protocol
				, SM.maybe "" cleanedCase gamemod
				])

	on view cursorChanged $ do
		(path, _) <- treeViewGetCursor view
		setServer False =<< getElementPath gen path

	on view rowActivated $ \path _ ->
		setServer True =<< getElementPath gen path

	scrollview <- scrolledWindowNew Nothing Nothing
	scrolledWindowSetPolicy scrollview PolicyNever PolicyAlways
	containerAdd scrollview view

	let updateF PollResult{..} = do
		listStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend raw) polled
		labelSetText statTot       $ show serversResponded
		labelSetText statRequested $ show (serversRequested - serversResponded)
		labelSetText statNow . show =<< treeModelIterNChildren filtered Nothing

	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0

	return (box, updateF)
	where
	showGame GameServer{..} = B.concat (protoToAbbr protocol : SM.maybe [] ((\x -> ["-",x]) . original) gamemod)
	showPlayers GameServer{..} = printf "%d / %2d" nplayers slots

browserUpdateOne :: BrowserStore -> GameServer -> IO ()
browserUpdateOne raw gs = treeModelForeach raw $ \iter -> do
	let i = listStoreIterToIndex iter
	e <- listStoreGetValue raw i
	if address e == address gs
		then listStoreSetValue raw i gs >> return True
		else return False

rememberColumn :: Bundle -> Int -> TreeViewColumn -> IO ()
rememberColumn Bundle{..} n col = do
	order <- treeViewColumnGetSortOrder col
	current <- takeMVar mconfig
	let new = current {browserSortColumn = n, browserOrder = order}
	putMVar mconfig new
	configToFile parent new
