module Favorites where
import Graphics.UI.Gtk

import Data.IORef
import Network.Tremulous.Protocol
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Types
import GtkUtils
import FindPlayers (playerLikeList)
import Constants
import Config

type ClanID = Int
data Favorite = Favorite
	{ favoritePlayers	:: ![ByteString]
	, favoriteClans		:: ![ClanID]
	, favoriteServers	:: ![Int]
	}


favlist = ["gt", "meisseli", ".gm"]

newFavorites:: Bundle -> SetCurrent -> IO VBox
newFavorites bundle@Bundle{..} setCurrent = do
	Config {..} <- atomically $ readTMVar mconfig
	gen@(GenFilterSort raw filtered sorted view) <- playerLikeList bundle setCurrent

	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(item, GameServer{..}) <- treeModelGetRow raw iter
		--s <- readIORef current
		return $ any (`B.isInfixOf` cleanedCase item) favlist


	scroll <- scrollIt view PolicyAutomatic PolicyAutomatic

	box <- vBoxNew False 0
	boxPackStart box scroll PackGrow 0

	return box
