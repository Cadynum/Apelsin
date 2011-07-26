module Clanlist (newClanList, newClanSync, newOnlineClans) where

import Graphics.UI.Gtk

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Ord
import Data.Tree
import Data.Maybe
import Data.List (sortBy)
import Data.IORef
import Network.Tremulous.NameInsensitive
import Network.Tremulous.ByteStringUtils
import qualified Network.Tremulous.Protocol as P
import Network.Tremulous.Util

import Types
import ClanFetcher
import Constants
import Config
import FilterBar
import InfoBox
import GtkUtils
import TremFormatting
import Monad2


newClanList :: Bundle -> [Clan] -> SetCurrent -> IO (VBox, ClanPolledHook, Entry)
newClanList Bundle{..} cache setCurrent = do
	raw			<- listStoreNew []
	filtered		<- treeModelFilterNew raw []
	sorted			<- treeModelSortNewWithModel filtered
	view			<- treeViewNewWithModel sorted
	scrollview		<- scrollIt view PolicyAutomatic PolicyAlways

	(infobox, statNow, statTot) <- newInfobox "clans"

	(filterbar, current, ent)	<- newFilterBar filtered statNow ""

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
		let cmplist	= [ cleanedCase name, tagExprGet tagexpr ]
		return $ B.null s || smartFilter s cmplist

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


newOnlineClans :: Bundle-> SetCurrent -> IO (ScrolledWindow, ClanPolledHook)
newOnlineClans Bundle{..} setServer = do
	Config {colors} <- atomically $ readTMVar mconfig

	let showName c =  case c of
		Left Clan{..}		-> "<b>" ++ htmlEscape (unpackorig name) ++ "</b>"
		Right (name, _)		-> pangoPretty colors name

	let showServer c =  case c of
		Left _ -> ""
		Right (_, P.GameServer{hostname}) -> pangoPretty colors hostname

	raw	<- treeStoreNew []
	view	<- treeViewNewWithModel raw

	treeViewExpandAll view
	addColumns raw view [
		  ("Name"	, 0	, True	, True	, True	, showName )
		, ("Server"	, 0	, True	, True	, True	, showServer )
		]

	let updateF clans P.PollResult{..} = do
		let players = buildTree $ sortByPlayers $
			associatePlayerToClans (makePlayerNameList polled) clans
		treeStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (treeStoreInsertTree raw [] 0) players
		treeViewExpandAll view

	on view cursorChanged $ do
		(path, _)	<- treeViewGetCursor view
		item		<- getElement raw path
		case item of
			Left _ -> return ()
			Right (_, a) -> setServer False a

	on view rowActivated $ \path _ -> do
		gs		<- getElement raw path
		case gs of
			Left _ -> return ()
			Right (_, gameserver) -> setServer True gameserver

	scroll <- scrollIt view PolicyAutomatic PolicyAutomatic

	return (scroll, updateF)


-- /// Utility functions ///////////////////////////////////////////////////////////////////////////

type PlayerList = [(TI, P.GameServer)]

type OnlineView = Forest (Either Clan (TI, P.GameServer))

associatePlayerToClans :: PlayerList -> [Clan] -> [(Clan, PlayerList)]
associatePlayerToClans players clans = map f clans
	where
	f c@Clan{tagexpr} = (c, filter (cmp tagexpr) players)
	cmp e = matchTagExpr e . fst

buildTree :: [(Clan, PlayerList)] -> OnlineView
buildTree = filter notEmpty . foldr f [] where
	f (clan, pls) xs = Node (Left clan) (map rightNode pls) : xs
	rightNode x = Node (Right x) []
	notEmpty (Node _ [])	= False
	notEmpty _		= True

sortByPlayers :: [(Clan, [b])] -> [(Clan, [b])]
sortByPlayers = sortBy (flip (comparing (\(a, b) -> (-length b, name a))))

newClanSync :: Bundle -> Button -> [ClanHook] -> [ClanPolledHook] -> IO ()
newClanSync Bundle{..} button clanHook bothHook = do
	set button [ widgetSensitive := False ]
	Config {clanSyncURL} <- atomically $ readTMVar mconfig
	forkIO $ do
		new <- getClanList clanSyncURL
		case new of
			Nothing	-> postGUISync $ do
				gtkError "Unable to download clanlist"
				set button [ widgetSensitive := True ]
			Just a	-> do
				result <- atomically $ do
					swapTMVar mclans a
					readTMVar mpolled
				postGUISync $ do
					mapM_ ($ a) clanHook
					mapM_ (\f -> f a result) bothHook
					set button [ widgetSensitive := True ]
	return ()
