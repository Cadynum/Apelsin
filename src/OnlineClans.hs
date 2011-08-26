module OnlineClans (newOnlineClans) where
import Graphics.UI.Gtk

import Data.Monoid
import Data.Ord
import Data.Tree
import Data.List (sortBy)
import Network.Tremulous.NameInsensitive
import qualified Network.Tremulous.Protocol as P
import Network.Tremulous.Util
import qualified Data.ByteString.Char8 as B

import ClanFetcher
import Types
import Config
import GtkUtils
import GtkExts
import TremFormatting

newOnlineClans :: Bundle-> SetCurrent -> IO (ScrolledWindow, ClanPolledHook)
newOnlineClans Bundle{..} setServer = do
	Config {colors} <- atomically $ readTMVar mconfig

	gen@(GenSimple raw view) <- newGenSimple =<< treeStoreNew []

	addColumn gen "Name" True [cellTextEllipsize := EllipsizeEnd] $ \rend ->
		cellSetMarkup rend . showName colors
	addColumn gen "Server" True [cellTextEllipsize := EllipsizeEnd] $ \rend ->
		cellSetMarkup rend . showServer colors

	let updateF clans P.PollResult{..} = do
		let players = buildTree $ sortByPlayers $
			associatePlayerToClans (makePlayerNameList polled) clans
		treeStoreClear raw
		treeViewColumnsAutosize view
		mapM_ (treeStoreInsertTree raw [] 0) players
		treeViewExpandAll view

	on view cursorChanged $ do
		(path, _)	<- treeViewGetCursor view
		item		<- getElementPath gen path
		case item of
			Left _ -> return ()
			Right (_, gs) -> setServer False gs

	on view rowActivated $ \path _ -> do
		item		<- getElementPath gen path
		case item of
			Left _ -> return ()
			Right (_, gs) -> setServer True gs

	scroll <- scrollIt view PolicyAutomatic PolicyAutomatic

	return (scroll, updateF)
	where
	showName colors c =  case c of
		Left Clan{..}		-> B.concat ["<b>", htmlEscapeBS $ original name, "</b>"]
		Right (name, _)		-> pangoPrettyBS colors name

	showServer colors c =  case c of
		Left _ -> ""
		Right (_, P.GameServer{hostname}) -> pangoPrettyBS colors hostname


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
sortByPlayers = sortBy $ comparing (length . snd) `mappend` flip (comparing (name . fst))
