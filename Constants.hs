module Constants where
import Data.String
import System.Environment.XDG.BaseDir
import System.FilePath.Posix ((</>))
import System.Directory
import Paths_apelsin

configName, programName, fullProgramName :: IsString s => s
configName = "apelsin"
programName = "Apelsin"
fullProgramName = "Apelsin Tremulous Browser"

inCacheDir, inConfDir, fromDataDir :: FilePath -> IO FilePath

inCacheDir x = do
	dir <- getUserCacheDir configName
	createDirectoryIfMissing True dir
	return (dir </> x)

inConfDir x = do
	dir <- getUserConfigDir configName
	createDirectoryIfMissing True dir
	return (dir </> x)

fromDataDir = getDataFileName 

spacing, spacingHalf, spacingBig :: Integral i => i
spacing		= 4
spacingHalf	= 2
spacingBig	= 8

