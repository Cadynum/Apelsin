module ServerBrowser where
import Graphics.UI.Gtk
import Control.Concurrent
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
newServerBrowser Bundle{..} setServer = do
	Config {..}	<- readMVar mconfig
	gen@(GenFilterSort raw filtered sorted view) <- newGenFilterSort browserStore

	addColumnFS gen "_Game" False
		(Just (comparing protocol `mappend` comparing gamemod `mappend` comparing gameping))
		cellRendererTextNew
		[]
		(\rend -> cellSetText rend . showGame)

	addColumnFS gen "_Name" True
		(Just (comparing hostname `mappend` comparing gameping))
		cellRendererTextNew
		[cellTextEllipsize := EllipsizeEnd]
		(\rend -> cellSetMarkup rend . pangoPrettyBS colors . hostname)

	addColumnFS gen "_Map" False
		(Just (comparing mapname `mappend` comparing gameping))
		cellRendererTextNew []
		(\rend -> cellSetText rend . SM.maybe "" (B.take 16 . original) . mapname)

	addColumnFS gen "P_ing" False
		(Just (comparing gameping `mappend` comparing hostname))
		cellRendererTextNew
		[cellXAlign := 1]
		(\rend x -> set rend [cellText := show $ gameping x])

	addColumnFS gen "_Players" False
		(Just (comparing nplayers `mappend` flip (comparing gameping)))
		cellRendererTextNew
		[cellXAlign := 1]
		(\rend x -> set rend [cellText := showPlayers x])


	treeSortableSetSortColumnId sorted browserSort browserOrder

	(infobox, statNow, statTot, statRequested) <- newInfoboxBrowser

	(filterbar, current) <- newFilterBar parent filtered statNow filterBrowser
	empty <- checkButtonNewWithMnemonic "_empty"
	set empty [ toggleButtonActive := filterEmpty ]
	boxPackStart filterbar empty PackNatural spacingHalf
	on empty toggled $ do
		treeModelFilterRefilter filtered
		labelSetText statNow . show =<< treeModelIterNChildren filtered Nothing


	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		GameServer{..}	<- treeModelGetRow raw iter
		s		<- readIORef current
		showEmpty	<- toggleButtonGetActive empty
		return $ (showEmpty || not (null players)) &&
			(smartFilter s
				[ cleanedCase hostname
				, SM.maybe "" cleanedCase mapname
				, SM.maybe "" cleanedCase version
				, proto2string protocol
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
	showGame GameServer{..} = B.concat (proto2string protocol : SM.maybe [] ((\x -> ["-",x]) . original) gamemod)
	showPlayers GameServer{..} = printf "%d / %2d" nplayers slots

browserUpdateOne :: BrowserStore -> GameServer -> IO ()
browserUpdateOne raw gs = treeModelForeach raw $ \iter -> do
	let i = listStoreIterToIndex iter
	e <- listStoreGetValue raw i
	if address e == address gs
		then listStoreSetValue raw i gs >> return True
		else return False
