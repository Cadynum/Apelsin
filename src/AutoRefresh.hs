module AutoRefresh (AutoSignal(..), autoSignal, autoRunner) where
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import Network.Tremulous.MicroTime
import Monad2
import Config

data AutoSignal = AutoUpdate | AutoStop | AutoStart | AutoPause | AutoResume
	deriving Show

data State = Running !ThreadId | Paused | Stopped

autoSignal :: MVar AutoSignal -> AutoSignal -> IO ()
autoSignal = putMVar

autoRunner :: MVar AutoSignal -> MVar Config -> IO a -> IO ThreadId
autoRunner m mconfig refreshAction = forkIO $ go Stopped Nothing
	where
	go :: State -> Maybe MicroTime -> IO a
	go state timestamp = do
		st <- takeMVar m
		case st of
			AutoStart -> case state of
				Running _ -> go state timestamp
				_ -> do	tid <- startAuto mconfig timestamp refreshAction
					go (Running tid) timestamp

			AutoStop -> case state of
				Running tid -> killThread tid >> go Stopped timestamp
				_ -> go Stopped timestamp

			AutoUpdate -> do
				timestamp' <- Just <$> getMicroTime
				case state of
					Running tid -> do
						killThread tid
						tid' <- startAuto mconfig timestamp' refreshAction
						go (Running tid') timestamp'
					_ -> go state timestamp'

			AutoPause | Running tid <- state -> do
				killThread tid
				go Paused timestamp

			AutoResume | Paused <- state -> do
				tid <- startAuto mconfig timestamp refreshAction
				go (Running tid) timestamp

			_ -> go state timestamp


startAuto :: MVar Config -> Maybe MicroTime -> IO a -> IO ThreadId
startAuto mconfig lastRefresh refreshAction = uninterruptibleMask $ \restore -> forkIO $ do
		delay <- autoRefreshDelay <$> readMVar mconfig
		whenJust lastRefresh $ \past -> do
			now <- getMicroTime
			when (past+delay > now) $
				restore $ threadDelay (fromIntegral (past+delay-now))
		refreshAction
		return ()
