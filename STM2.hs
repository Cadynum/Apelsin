module STM2(
	module Control.Concurrent, module Control.Concurrent.STM
	, tryReadTMVar, withTMVar, clearTMVar, replaceTMVar
) where

import Control.Concurrent
import Control.Concurrent.STM

tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar v = do 
	x <- tryTakeTMVar v
	case x of
		Just a	-> putTMVar v a >> return (Just a)
		Nothing	-> return Nothing

withTMVar :: TMVar a -> (a -> IO ()) -> IO ()
withTMVar mvar f = do
	tst <- atomically $ tryReadTMVar mvar
	case tst of
		Nothing	-> return ()
		Just a	-> f a
		
clearTMVar :: TMVar a -> STM ()		
clearTMVar mvar = do
	t <- isEmptyTMVar mvar
	if t 
	then return ()
	else takeTMVar mvar >> return ()

replaceTMVar :: TMVar a -> a -> STM ()
replaceTMVar t x = clearTMVar t >> putTMVar t x
