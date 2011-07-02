module Clanlist where

import Graphics.UI.Gtk

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Data.Ord
import Data.Tree
import Data.List (sortBy)
import Data.IORef 
import Network.Tremulous.NameInsensitive
import qualified Network.Tremulous.Protocol as P
import Network.Tremulous.Util

import ClanFetcher
import Constants
import Config
import FilterBar
import InfoBox
import GtkUtils
import TremFormatting


newClanList :: TMVar [Clan] -> IO (VBox, IO ())
newClanList mclans = do
	raw			<- listStoreNew []
	filtered		<- treeModelFilterNew raw []
	sorted			<- treeModelSortNewWithModel filtered	
	view			<- treeViewNewWithModel sorted
	scrollview		<- scrollIt view PolicyAutomatic PolicyAlways
	
	(infobox, statNow, statTot) <- newInfobox "clans"

	(filterbar, current)	<- newFilterBar filtered statNow ""
	
	let updateF = do
		new <- atomically $ readTMVar mclans
		listStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (listStoreAppend raw) new
		treeModelFilterRefilter filtered
		set statTot [ labelText := show (length new) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]
		
	addColumnsFilterSort raw filtered sorted view (Just (comparing name)) [
		  ("_Name"	, False	, True	, False	, unpackorig . name	, Just (comparing name))
		, ("_Tag"	, False	, True	, False	, unpackorig . tag	, Just (comparing tag))
		, ("Website"	, False	, True	, False	, B.unpack . website	, Nothing)
		, ("IRC"	, False	, True	, False	, B.unpack . irc	, Nothing)
		]
	
	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		Clan{..}	<- treeModelGetRow raw iter
		s		<- readIORef current
		let cmplist	= [ cleanedCase name, cleanedCase tag ]
		return $ B.null s || smartFilter s cmplist
	
	box <- vBoxNew False 0
	
	boxPackStart box filterbar PackNatural g_SPACING
	boxPackStart box scrollview PackGrow 0
	boxPackStart box infobox PackNatural 0

	updateF
	
	return (box, updateF)
		
--newOnlineClans:: TMVar [P.GameServer] -> TMVar [Clan] -> (P.GameServer -> IO ()) -> IO (ScrolledWindow, IO ())
newOnlineClans mvar mconfig mclans setCurrent = do
	Config {colors} <- atomically $ readTMVar mconfig

	let showName c =  case c of
		Left Clan{..}		-> "<b>" ++ htmlEscape (unpackorig name) ++ "</b>"
		Right (P.Player{..}, _)	-> pangoPretty colors name
	
	let showServer c =  case c of
		Left _ -> ""
		Right (_, P.GameServer{hostname}) -> unfuckName colors hostname
	
	raw	<- treeStoreNew []
	view	<- treeViewNewWithModel raw
	
	
	treeViewExpandAll view
	addColumns raw view [
		  ("Name"	, True	, True	, True	, showName )
		, ("Server"	, True	, True	, True	, showServer )
		]
		
	let updateF = do
		polled	<- atomically $ readTMVar mvar
		clans	<- atomically $ readTMVar mclans
		let players = buildTree $sortByPlayers $
			associatePlayerToClans (toPlayerList polled) clans
		treeStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (treeStoreInsertTree raw [] 0) players
		treeViewExpandAll view
		
	onCursorChanged view $ do
		(x, _)		<- treeViewGetCursor view
		Just iter	<- treeModelGetIter raw x
		item		<- treeModelGetRow raw iter
		case item of
			Left _ -> return ()
			Right (_, a) -> setCurrent a
		
	scroll <- scrollIt view PolicyAutomatic PolicyAutomatic
	
	
	return (scroll, updateF)
	
	



-- /// Utility functions ///////////////////////////////////////////////////////////////////////////

type PlayerList = [(P.Player, P.GameServer)]

type OnlineView = Forest (Either Clan (P.Player, P.GameServer))

associatePlayerToClans :: PlayerList -> [Clan] -> [(Clan, PlayerList)]
associatePlayerToClans players clans = map f clans
	where 
	f c@Clan{tag} = (c, filter (cmp tag) players)
	cmp tag x = cleanedCase tag `B.isInfixOf` (cleanedCase . P.name . fst) x

buildTree :: [(Clan, PlayerList)] -> OnlineView
buildTree = filter notEmpty . foldr f [] where
	f (clan, pls) xs = Node (Left clan) (map rightNode pls) : xs
	rightNode x = Node (Right x) []
	notEmpty (Node _ [])	= False
	notEmpty _		= True
--f tag x = 

sortByPlayers = sortBy (comparing (length . snd))

newClanSync mconfig mclans updateF = do
	button	<- buttonNewWithMnemonic "_Sync clan list"
	img	<- imageNewFromStock stockSave IconSizeButton
	set button 	[ buttonImage := img 
			, buttonRelief := ReliefNone 
			, buttonFocusOnClick := False ]

	box	<- hBoxNew False 0
	boxPackStart box button PackRepel 0
	
	
	let doSync = do
		set button [ widgetSensitive := False ]
		Config {clanSyncURL} <- atomically $ readTMVar mconfig
		forkIO $ do
			new <- getClanList clanSyncURL
			case new of
				Nothing	-> putStrLn "Error fetching clanlist!"
				Just a	-> do 
					atomically $ 
						swapTMVar mclans a
					postGUISync $ do
						updateF
						set button [ widgetSensitive := True ]		
		return ()
		
	on button buttonActivated doSync
				
	return (box, doSync)
