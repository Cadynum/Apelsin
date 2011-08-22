module Favorites where
import Graphics.UI.Gtk

import Data.IORef
import Network.Tremulous.Protocol
import qualified Data.ByteString as B

import Types
import GtkUtils
import ClanFetcher
import FindPlayers (playerLikeList)
import Constants
import Config


favlist = ["gt", "meisseli", ".gm"]

newFavorites:: Bundle -> SetCurrent -> IO VBox
newFavorites bundle@Bundle{..} setCurrent = do
	favPlayers	<- newIORef (map mk favlist)
	favClans	<- newIORef [1]
	favServers	<- newIORef []


	playersLabel <- labelNew (Just "Players")
	labelSetAttributes playersLabel [AttrWeight 0 (-1) WeightBold]
	set playersLabel [miscXalign := 0, miscXpad := spacingHalf]

	gen@(GenFilterSort raw filtered sorted view) <- playerLikeList bundle setCurrent
	scroll <- scrollIt view PolicyAutomatic PolicyAutomatic

	clansx <- atomically $ readTMVar mclans
	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(item, GameServer{..}) <- treeModelGetRow raw iter
		favP <- readIORef favPlayers
		favC <- readIORef favClans
		return $ any (\x -> cleanedCase x `B.isInfixOf` cleanedCase item) favP
		      || any (matchClanByID clansx item) favC


	box <- vBoxNew False 0
	boxPackStart box playersLabel PackNatural spacingBig
	boxPackStart box scroll PackGrow 0

	return box
