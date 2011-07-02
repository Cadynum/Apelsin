module Toolbar where

import Graphics.UI.Gtk

import Control.Applicative
import Control.DeepSeq
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

	on about buttonActivated $ newAbout
			
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
		let tremTime = (resendTimes + 1) * resendWait
		pbth <- forkIO $ whileTrue $ do
		
			threadDelay 100000 --100 ms
			now <- getMicroTime
			let diff = now - start
			if now-start > fromIntegral tremTime then do
				progressBarSetFraction pb 1
				return False
			else do
				progressBarSetFraction pb
					(fromIntegral diff / fromIntegral tremTime)
				return True
			
		forkIO $ do
			Config {delays} <- atomically $ readTMVar mconfig
			hosts <- catMaybes <$> mapM
				(\(host, port, proto) -> fmap (MasterServer proto) <$> getDNS host (show port))
				masterServers
			(polled,_,_,_) <- pollMasters delays hosts
			-- Force evaluation in this thread to prevent blocking in the mainthread
			polled `deepseq` return ()
			atomically $ clearTMVar mpolled >> putTMVar mpolled polled
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
	
