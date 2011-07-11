{-# LANGUAGE CPP #-}
module Constants where
import Data.String
import System.Environment.XDG.BaseDir
import System.FilePath
import System.Directory
import System.IO
import System.Process

#ifdef CABAL_PATH
import Paths_apelsin
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


defaultTremulousPath, defaultTremulousGPPPath:: FilePath
trace :: String -> IO ()
defaultBrowser :: String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
defaultTremulousPath	= "C:\\Program Files\\Tremulous\\tremulous.exe"
defaultTremulousGPPPath	= "C:\\Program Files\\Tremulous\\tremulous-gpp.exe"
trace _			= return ()
defaultBrowser		= "start"
#else
defaultTremulousPath	= "tremulous"
defaultTremulousGPPPath	= "tremulous-gpp"
trace 			= hPutStrLn stderr
defaultBrowser		= "xdg-open"
#endif

openInBrowser :: String -> IO ()
openInBrowser x = do
	createProcess (proc defaultBrowser [x])
		{close_fds = True, std_in = Inherit, std_out = Inherit, std_err = Inherit}
	return ()


spacing, spacingHalf, spacingBig :: Integral i => i
spacing		= 4
spacingHalf	= 2
spacingBig	= 8

