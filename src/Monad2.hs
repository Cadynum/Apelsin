module Monad2 where
import Control.Monad

whileTrue :: Monad m => m Bool -> m ()
whileTrue f = f >>= \t -> when t (whileTrue f)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = case x of
	Nothing	-> return ()
	Just a	-> f a

whenM :: Monad m => m Bool -> m () -> m ()
whenM c m = c >>= \t -> if t then m else return ()
