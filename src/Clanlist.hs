module Clanlist (newClanList) where
import Graphics.UI.Gtk

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


newClanList :: Bundle -> [Clan] -> SetCurrent -> IO (VBox, ClanPolledHook)
newClanList Bundle{..} cache setCurrent = do
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
		cellRendererPixbufNew haveServer
	addColumnFS gen "_Name" False (Just $ comparing $ name . fst)
		cellRendererTextNew (simpleColumn (original . name))
	addColumnFS gen "_Tag" False (Just $ comparing $ tagexpr . fst)
		cellRendererTextNew (markupColumn (prettyTagExpr . tagexpr))
	addColumnFS gen "Website" False Nothing
		cellRendererTextNew (simpleColumn (showURL . website))
	addColumnFS gen "IRC" False Nothing
		cellRendererTextNew (simpleColumn irc)

	treeSortableSetSortColumnId sorted 1 SortAscending
		

	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(Clan{..}, _)	<- treeModelGetRow raw iter
		s		<- readIORef current
		return $ smartFilter s	[ cleanedCase name
					, cleanedCase (tagExprGet tagexpr) ]

	on view cursorChanged $ do
		(path, _)		<- treeViewGetCursor view
		(Clan{..}, active)	<- getElementPath gen path
		
		when active $ whenJust clanserver $ \server -> do
			P.PollResult{..} <- atomically $ readTMVar mpolled
			whenJust (serverByAddress server polled) (setCurrent False)

	on view rowActivated $ \path _ -> do
		(Clan{..}, _) <- getElementPath gen path
		unless (B.null website) $
			openInBrowser (B.unpack website)

	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0

	
	updateF cache =<< atomically (readTMVar mpolled)

	return (box, updateF)
	where
	showURL x = SM.fromMaybe x (stripPrefix "http://" x)
	markupColumn f rend (item, _) = cellSetMarkup rend (f item)
	simpleColumn f rend (item, _) = cellSetText rend (f item)
	haveServer rend (Clan{..}, active) = set rend $case clanserver of
		Just _	-> [cellPixbufStockId := stockNetwork, cellSensitive := active]
		Nothing	-> [cellPixbufStockId := ""]
