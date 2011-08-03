module Exception2 where
import Control.Exception

maybeIO :: IO (Maybe a) -> IO (Maybe a)
maybeIO = handleIOWith Nothing

tryIO :: IO a -> IO Bool
tryIO f = handleIOWith False (f >> return True)

ignoreIOException :: IO () -> IO ()
ignoreIOException = handleIOWith ()

handleIOWith :: a -> IO a -> IO a
handleIOWith w = handle (\(_ :: IOException) -> return w)
