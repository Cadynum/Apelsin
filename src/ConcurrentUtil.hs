module ConcurrentUtil where

import Control.Concurrent
import Control.Exception

readWithMVar :: MVar m -> (m -> IO ()) -> IO ()
readWithMVar m f = mask $ \restore -> do
	ret <- tryTakeMVar m
	case ret of
		Just a  -> putMVar m a >> restore (f a)
		Nothing -> return ()
