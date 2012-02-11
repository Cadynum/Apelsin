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

autoSignal :: MVar AutoSignal -> AutoSignal -> IO ()
autoSignal m = putMVar m

autoRunner :: MVar AutoSignal -> MVar Config -> IO a -> IO ThreadId
autoRunner m mconfig refreshAction = forkIO $ go False Nothing Nothing
	where
	go :: Bool -> Maybe ThreadId -> Maybe MicroTime -> IO a
	go active tid timestamp = do
		st <- takeMVar m
		print st
		case st of
			AutoStart  -> do
				tid' <- case tid of
					Just a -> return (Just a)
					Nothing -> Just <$> startAuto mconfig timestamp refreshAction
				go True tid' timestamp
			AutoStop -> do
				whenJust tid killThread
				go False Nothing timestamp
			AutoUpdate -> do
				timestamp' <- Just <$> getMicroTime
				case timestamp of
					Nothing -> do
						tid' <- if active
							then Just <$> startAuto mconfig timestamp' refreshAction
							else return tid
						go active tid' timestamp'
					Just _ | active -> do
						whenJust tid killThread
						tid' <- startAuto mconfig timestamp' refreshAction
						go active (Just tid') timestamp'
					_ -> go active tid timestamp'
			AutoPause | Just a <- tid -> do
				killThread a
				go active Nothing timestamp
			AutoResume | active -> do
				tid' <- startAuto mconfig timestamp refreshAction
				go active (Just tid') timestamp
			_ -> go active tid timestamp


startAuto :: MVar Config -> Maybe MicroTime -> IO a -> IO ThreadId
startAuto mconfig lastRefresh refreshAction = uninterruptibleMask $ \restore -> forkIO $ do
		delay <- autoDelay <$> readMVar mconfig
		whenJust lastRefresh $ \past -> do
			now <- getMicroTime
			when (past+fromIntegral delay > now) $
				restore $ threadDelay (fromIntegral (past+fromIntegral delay-now))
		refreshAction
		return ()
