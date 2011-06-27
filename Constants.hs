module Constants where
import System.Environment.XDG.BaseDir
import System.FilePath.Posix ((</>))
import System.Directory

configName = "apelsin"
programName = "Apelsin"
fullProgramName = "Apelsin Tremulous Browser"

inCacheDir x = do
	dir <- getUserCacheDir configName
	createDirectoryIfMissing True dir
	return (dir </> x)

inConfDir x = do
	dir <- getUserConfigDir configName
	createDirectoryIfMissing True dir
	return (dir </> x)

g_SPACING, spacingHalf, spacingBig :: Integral i => i
g_SPACING = 4
spacingHalf = 2
spacingBig = 8

