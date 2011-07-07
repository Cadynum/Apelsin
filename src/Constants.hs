{-# LANGUAGE CPP #-}
module Constants where
import Data.String
import System.Environment.XDG.BaseDir
import System.FilePath
import System.Directory
import System.IO

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
getDataDir = (</> "data") `fmap` getCurrentDirectory
#endif

trace :: String -> IO ()
defaultTremulousPath, defaultTremulousGPPPath :: FilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
defaultTremulousPath	= "C:\\Program Files\\Tremulous\\tremulous.exe"
defaultTremulousGPPPath	= "C:\\Program Files\\Tremulous\\tremulous-gpp.exe"
trace _			= return ()
#else
defaultTremulousPath	= "tremulous"
defaultTremulousGPPPath	= "tremulous-gpp"
trace 			= hPutStrLn stderr
#endif


spacing, spacingHalf, spacingBig :: Integral i => i
spacing		= 4
spacingHalf	= 2
spacingBig	= 8

