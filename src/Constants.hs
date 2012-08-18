{-# LANGUAGE CPP #-}
module Constants where
import Data.String
import System.Environment.XDG.BaseDir
import System.FilePath
import System.Directory
import System.IO
import System.Process
import Control.Concurrent
import Control.Exception
import Control.Monad

#ifdef CABAL_PATH
import Paths_apelsin hiding (getDataDir)
#endif

configName, programName, fullProgramName :: IsString s => s
configName = "apelsin"
programName = "Apelsin"
fullProgramName = "Apelsin Tremulous Browser"

inCacheDir, inConfDir :: FilePath -> IO FilePath

inCacheDir x = do
	dir <- getUserCacheDir configName
	createDirectoryIfMissing True dir
	return (dir </> x)

inConfDir x = do
	dir <- getUserConfigDir configName
	createDirectoryIfMissing True dir
	return (dir </> x)


getDataDir :: IO FilePath
#ifdef CABAL_PATH
getDataDir = dropTrailingPathSeparator `fmap` getDataFileName ""
#else
getDataDir = getCurrentDirectory
#endif


trace :: String -> IO ()
defaultBrowser :: String -> IO CreateProcess

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
trace _			= return ()
defaultBrowser x	= do
	ddir <- getDataDir
	return (proc "wscript" [ddir </> "open.js", x])

#else
trace x			= hPutStrLn stderr x >> hFlush stderr

#if defined(__APPLE__) || defined(__MACH__)
defaultBrowser	x	= return $ proc "open" [x]
#else
defaultBrowser	x	= return $ proc "xdg-open" [x]
#endif

#endif

openInBrowser :: String -> IO ()
openInBrowser x = handle (\(_ :: IOError) -> return ()) $ do
	p <- defaultBrowser x
	(_,_,_,hdl) <- createProcess p {close_fds = True}
	-- The following hack is needed to avoid ghost processes because of:
	-- http://hackage.haskell.org/trac/ghc/ticket/2123
	forkIO $ void $ waitForProcess hdl
	return ()

spacing, spacingHalf, spacingBig, spacingHuge :: Integral i => i
spacingHalf	= 2
spacing		= 4
spacingBig	= 8
spacingHuge	= 12

