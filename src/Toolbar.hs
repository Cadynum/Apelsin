module Toolbar where

import Graphics.UI.Gtk

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe

import Network.Socket 
import Network.Tremulous.Protocol
import Network.Tremulous.Polling
import Network.Tremulous.Scheduler (getMicroTime)

import Types
import Constants
import STM2

import Config
import About
import Clanlist

getDNS :: String -> String -> IO (Maybe SockAddr)
getDNS host port = handle (\(_::IOException) -> return Nothing) $ do
	AddrInfo _ _ _ _ addr _ <- Prelude.head `liftM` getAddrInfo Nothing (Just host) (Just port)
	return $ Just addr

	
whileTrue :: Monad m => m Bool -> m ()
whileTrue f = f >>= \t -> when t (whileTrue f)

newToolbar :: Bundle -> IO () -> IO () -> IO HBox
newToolbar bundle@Bundle{..} clanHook polledHook = do	
	pbrbox		<- hBoxNew False spacing
		
	pb		<- progressBarNew
	set pb		[ widgetNoShowAll := True ]
	
	refresh	<- buttonNewWithMnemonic "_Refresh all servers"
	rimg		<- imageNewFromStock stockRefresh IconSizeButton
	set refresh	[ buttonImage := rimg 
			, buttonRelief := ReliefNone 
			, buttonFocusOnClick := False ]
			
	about		<- buttonNewFromStock stockAbout
	set about	[ buttonRelief		:= ReliefNone 
			, buttonFocusOnClick	:= False ]

	on about buttonActivated $ newAbout parent
			
	(clanSync, doSync) <- newClanSync bundle clanHook
	
	align		<- alignmentNew 0 0 0 0
	alignbox	<- hBoxNew False spacing
	set align [ containerChild := alignbox ]
	
	boxPackStartDefaults alignbox refresh
	boxPackStartDefaults alignbox clanSync
	boxPackStartDefaults alignbox about
			
	
	set pbrbox [ containerBorderWidth := spacing ]
	boxPackStart pbrbox align PackNatural 0
	boxPackStart pbrbox pb PackGrow 0
	
	
	let serverRefresh = do
		progressBarSetFraction pb 0
		widgetShow pb
		refresh `set` [ widgetSensitive := False ]
		Config {masterServers, delays=Delay{..}}	<- atomically $ readTMVar mconfig
		
		start <- getMicroTime

		-- This is a stupid guess based on that about 110 servers will respond and the master
		-- will take about 200ms to respond
		let tremTime = (packetDuplication + 1) * packetTimeout
			+ 100 * throughputDelay + 200 * 1000
		
		pbth <- forkIO $ whileTrue $ do
			threadDelay 10000 --10 ms, 100 fps
			now <- getMicroTime
			let diff = now - start
			if now-start > fromIntegral tremTime then do
				postGUISync $ progressBarSetFraction pb 1
				return False
			else do
				postGUISync $ progressBarSetFraction pb
					(fromIntegral diff / fromIntegral tremTime)
				return True
			
		forkIO $ do
			Config {delays} <- atomically $ readTMVar mconfig
			hosts <- catMaybes <$> mapM
				(\(host, port, proto) -> fmap (`MasterServer` proto) <$> getDNS host (show port))
				masterServers
			result <- pollMasters delays hosts

			atomically $ replaceTMVar mpolled result
			killThread pbth
			postGUISync $ do
				polledHook
				refresh `set` [ widgetSensitive := True ]
				widgetHide pb
		return ()
	on refresh buttonActivated serverRefresh

	Config {..} <- atomically $ readTMVar mconfig
	when autoMaster serverRefresh
	when autoClan doSync
	return pbrbox
	
