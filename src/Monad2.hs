module Monad2 where
import Control.Monad
import Network.Socket
import Control.Exception
import Control.Applicative
import Data.Maybe

whileTrue :: Monad m => m Bool -> m ()
whileTrue f = f >>= \t -> when t (whileTrue f)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = case x of
	Nothing	-> return ()
	Just a	-> f a

whenM :: Monad m => m Bool -> m () -> m ()
whenM c m = c >>= \t -> if t then m else return ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c m = c >>= \t -> if not t then m else return ()

getDNS :: String -> String -> IO (Maybe SockAddr)
getDNS host port = handle (\(_ :: IOException) -> return Nothing) $
	fmap addrAddress . listToMaybe <$> getAddrInfo Nothing (Just host) (Just port)
