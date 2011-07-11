module STM2(
	module Control.Concurrent, module Control.Concurrent.STM
	, tryReadTMVar, withTMVar, clearTMVar, replaceTMVar, modifyTMVar
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

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
	unless t $ do
		takeTMVar mvar
		return ()

replaceTMVar :: TMVar a -> a -> STM ()
replaceTMVar t x = clearTMVar t >> putTMVar t x

modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar x f = do
	t <- takeTMVar x
	putTMVar x (f t)
