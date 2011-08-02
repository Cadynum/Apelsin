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
import Network.Tremulous.Util

import Types
import ClanFetcher
import Constants
import FilterBar
import InfoBox
import GtkUtils
import Monad2


newClanList :: Bundle -> [Clan] -> SetCurrent -> IO (VBox, ClanPolledHook, Entry)
newClanList Bundle{..} cache setCurrent = do
	raw			<- listStoreNew []
	filtered		<- treeModelFilterNew raw []
	sorted			<- treeModelSortNewWithModel filtered
	view			<- treeViewNewWithModel sorted
	scrollview		<- scrollIt view PolicyAutomatic PolicyAlways

	(infobox, statNow, statTot) <- newInfobox "clans"

	(filterbar, current, ent) <- newFilterBar filtered statNow ""

	let updateF newraw P.PollResult{..} = do
		let new = (`map` newraw) $ \c -> case clanserver c of
			Nothing -> (c, False)
			Just a -> (c, a `elemByAddress` polled)
		listStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend raw) new
		treeModelFilterRefilter filtered
		set statTot [ labelText := show (length new) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]

	addColumnsFilterSort raw filtered sorted view 1 SortAscending 
		[ (""		, False	, RendPixbuf haveServer
			, Just (comparing (isJust . clanserver . fst)))
		, ("_Name"	, False	, RendText (simpleColumn (unpackorig . name))
			, Just (comparing (name . fst)))
		, ("_Tag"	, False	, RendText (markupColumn (prettyTagExpr . tagexpr))
			, Just (comparing (tagexpr . fst)))
		, ("Website"	, False	, RendText (simpleColumn (B.unpack . showURL . website))
			, Nothing)
		, ("IRC"	, False , RendText (simpleColumn (B.unpack . irc))
			, Nothing)
		]

	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(Clan{..}, _)	<- treeModelGetRow raw iter
		s		<- readIORef current
		let cmplist	= [ cleanedCase name, cleanedCase (tagExprGet tagexpr) ]
		return $ smartFilter s cmplist

	on view cursorChanged $ do
		(path, _)		<- treeViewGetCursor view
		(Clan{..}, active)	<- getElementFS raw sorted filtered path
		
		when active $ whenJust clanserver $ \server -> do
			P.PollResult{..} <- atomically $ readTMVar mpolled
			whenJust (serverByAddress server polled) (setCurrent False)

	on view rowActivated $ \path _ -> do
		(Clan{..}, _) <- getElementFS raw sorted filtered path
		unless (B.null website) $
			openInBrowser (B.unpack website)

	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0

	
	updateF cache =<< atomically (readTMVar mpolled)

	return (box, updateF, ent)
	where
	showURL x = fromMaybe x (stripPrefix "http://" x)
	markupColumn f (item, _) = [ cellTextMarkup := Just (f item) ]
	simpleColumn f (item, _) = [ cellText := f item ]
	haveServer (Clan{..}, active) = case clanserver of
		Just _	-> [cellPixbufStockId := stockNetwork, cellSensitive := active]
		Nothing	-> [cellPixbufStockId := ""]
