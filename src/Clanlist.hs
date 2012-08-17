module Clanlist (newClanList) where
import Graphics.UI.Gtk

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Ord
import Data.Maybe
import Data.IORef
import Network.Tremulous.NameInsensitive
import Network.Tremulous.ByteStringUtils
import qualified Network.Tremulous.Protocol as P
import qualified Network.Tremulous.StrictMaybe as SM
import Network.Tremulous.Util

import Types
import ClanFetcher
import Constants
import FilterBar
import InfoBox
import GtkUtils
import GtkExts
import Monad2
import Config


newClanList :: Bundle -> SetCurrent -> IO (VBox, ClanPolledHook)
newClanList bundle@Bundle{..} setCurrent = do
	gen@(GenFilterSort raw filtered sorted view)
		<- newGenFilterSort =<< listStoreNew []
	scrollview		<- scrollIt view PolicyAutomatic PolicyAlways

	(infobox, statNow, statTot) <- newInfobox "clans"

	(filterbar, current) <- newFilterBar parent filtered statNow ""

	let updateF newraw P.PollResult{..} = do
		let new = (`map` newraw) $ \c -> case clanserver c of
			Nothing -> (c, False)
			Just a -> (c, a `elemByAddress` polled)
		listStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend raw) new
		labelSetText statTot . show =<< treeModelIterNChildren raw Nothing
		labelSetText statNow . show =<< treeModelIterNChildren filtered Nothing

	addColumnFS gen "" False (Just $ comparing $ isJust . clanserver . fst)
		(rememberColumn bundle 0)
		cellRendererPixbufNew [] haveServer

	addColumnFS gen "_Name" False (Just $ comparing $ name . fst)
		(rememberColumn bundle 1)
		cellRendererTextNew [] (simpleColumn (original . name))

	addColumnFS gen "_Tag" False (Just $ comparing $ tagexpr . fst)
		(rememberColumn bundle 2)
		cellRendererTextNew [] (markupColumn (prettyTagExpr . tagexpr))

	addColumnFS gen "Website" False Nothing
		(const (return ()))
		cellRendererTextNew [] $ \rend (Clan{..},_) -> do
			set rend $ if websitealive
				then [ cellTextStrikethrough := False, cellTextUnderline := UnderlineSingle, cellTextForegroundColor := Color 0 0 maxBound ]
				else [ cellTextStrikethrough := True, cellTextUnderline := UnderlineNone, cellTextForegroundColor := Color maxBound 0 0 ]
			cellSetMarkup rend (showURL website)

	addColumnFS gen "IRC" False Nothing
		(const (return ()))
		cellRendererTextNew [] (simpleColumn irc)

	Config{..} <- readMVar mconfig
	treeSortableSetSortColumnId sorted clanlistSortColumn clanlistOrder


	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(Clan{..}, _)	<- treeModelGetRow raw iter
		s		<- readIORef current
		return $ smartFilter s	[ cleanedCase name
					, cleanedCase (tagExprGet tagexpr) ]

	on view cursorChanged $ do
		(path, col)		<- treeViewGetCursor view
		(Clan{..}, active)	<- getElementPath gen path
		title			<- maybe (return Nothing) treeViewColumnGetTitle col

		when (title == Just "Website" && (not . B.null) website) $ openInBrowser $
			if websitealive then B.unpack website
					else "http://wayback.archive.org/web/*/" ++ (B.unpack website)

		when active $ whenJust clanserver $ \server -> do
			P.PollResult{..} <- readMVar mpolled
			whenJust (serverByAddress server polled) (setCurrent False)

	on view rowActivated $ \path col -> do
		(Clan{..}, active) <- getElementPath gen path
		title <- treeViewColumnGetTitle  col
		case title of
			Just "Website" -> return ()
			_ -> when active $ whenJust clanserver $ \server -> do
				P.PollResult{..} <- readMVar mpolled
				whenJust (serverByAddress server polled) (setCurrent True)

	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0

	return (box, updateF)
	where
	showURL x = SM.fromMaybe x (stripPrefix "http://" x)
	markupColumn f rend (item, _) = cellSetMarkup rend (f item)
	simpleColumn f rend (item, _) = cellSetText rend (f item)
	haveServer rend (Clan{..}, active) = set rend $ case clanserver of
		Just _	-> [cellPixbufStockId := stockNetwork, cellSensitive := active]
		Nothing	-> [cellPixbufStockId := ""]


rememberColumn :: Bundle -> Int -> TreeViewColumn -> IO ()
rememberColumn Bundle{..} n col = do
	order <- treeViewColumnGetSortOrder col
	current <- takeMVar mconfig
	let new = current {clanlistSortColumn = n, clanlistOrder = order}
	putMVar mconfig new
	configToFile parent new
