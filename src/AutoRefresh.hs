module AutoRefresh (AutoSignal(..), autoSignal, autoRunner) where
import Control.Concurrent
import Control.Exception
import Network.Tremulous.MicroTime
import Control.Monad

import Monad2

data AutoSignal = AutoUpdate | AutoStop | AutoStart | AutoPause | AutoResume
	deriving Show

autoSignal :: MVar AutoSignal -> AutoSignal -> IO ()
autoSignal m = putMVar m

autoRunner :: MVar AutoSignal -> IO a -> IO ()
autoRunner m refreshAction = do
	mActive <- newMVar False
	mTid <- newEmptyMVar
	mTimestamp <- newEmptyMVar

	forkIO $ forever $ do
		st <- takeMVar m
		print st
		case st of
			AutoStart -> do
				swapMVar mActive True
				b <- isEmptyMVar mTid
				when b (startAuto mTimestamp mTid refreshAction)
			AutoStop -> do
				swapMVar mActive False
				test <- tryTakeMVar mTid
				whenJust test killThread
			AutoUpdate -> do
				test <- tryTakeMVar mTimestamp
				case test of
					Nothing -> do
						putMVar mTimestamp =<< getMicroTime
						isActive <- readMVar mActive
						when isActive $
							startAuto mTimestamp mTid refreshAction
					Just a -> do
						isActive <- readMVar mActive
						when isActive $ do
							putMVar mTimestamp a
							b <- tryTakeMVar mTid
							whenJust b killThread
							startAuto mTimestamp mTid refreshAction
			AutoPause -> do
				test <- tryTakeMVar mTid
				whenJust test killThread
			AutoResume -> do
				test <- readMVar mActive
				when test (startAuto mTimestamp mTid refreshAction)
	return ()

	--where
	--	go active tid timestamp =


startAuto :: MVar MicroTime -> MVar ThreadId -> IO a -> IO ()
startAuto lastRefresh mTid refreshAction = do
	tid <- uninterruptibleMask $ \restore -> forkIO $ do
		mlast <- tryTakeMVar lastRefresh
		whenJust mlast $ \past -> do
			now <- getMicroTime
			when (past+wait > now) $
				restore $ threadDelay (fromIntegral (past+wait-now))
		refreshAction
		return ()
	tryTakeMVar mTid
	putMVar mTid tid


--tmp
wait :: MicroTime
wait = 6000000
