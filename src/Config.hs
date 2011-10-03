{-# LANGUAGE StandaloneDeriving #-}
module Config where
import Graphics.UI.Gtk (SortType(..))
import Data.Array
import Control.Exception
import Control.Applicative
import Network.Tremulous.Protocol
import Network.Tremulous.TupleReader
import Network.Tremulous.StrictMaybe as SM

import Constants
import List2
import GtkUtils
import TremFormatting

deriving instance Read SortType

type ColorTheme = Array Int TremFmt

data Config = Config
	{ masterServers :: ![(String, Int, Int)]
	, clanSyncURL	:: !String
	, tremPath
	, tremGppPath	:: !String
	, autoMaster
	, autoClan
	, autoGeometry	:: !Bool
	, filterBrowser
	, filterPlayers	:: !String
	, filterEmpty	:: !Bool
	, browserSort	:: !Int
	, playersSort	:: !Int
	, browserOrder
	, playersOrder	:: !SortType
	, delays	:: !Delay
	, colors	:: !ColorTheme
	} deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config {
	  masterServers = [("master.tremulous.net", 30710, 69), ("master.tremulous.net", 30700, 70)]
	, clanSyncURL	= "http://ddos-tremulous.eu/cw/api/clanlist"
	, tremPath	= defaultTremulousPath
	, tremGppPath	= defaultTremulousGPPPath
	, autoMaster	= True
	, autoClan	= True
	, autoGeometry	= True
	, filterBrowser	= ""
	, filterPlayers	= ""
	, filterEmpty	= True
	, browserSort	= 3
	, playersSort	= 0
	, browserOrder	= SortAscending
	, playersOrder	= SortAscending
	, delays	= defaultDelay
	, colors	= makeColorsFromList $
		TFNone "#000000" : map TFColor ["#d60503", "#25c200", "#eab93d", "#0021fe", "#04c9c9", "#e700d7"] ++ [TFNone "#000000"]
	}
{-
newParse :: [(String, String)] -> SM.Maybe Config
newParse = tupleReader $ do
	masterServers	<- get "masterservers"	[("master.tremulous.net", 30710, 69), ("master.tremulous.net", 30700, 70)]
	clanSyncURL	<- get "clanlisturl"	"http://ddos-tremulous.eu/cw/api/clanlist"
	tremPath	<- get "tremulous1.1"	defaultTremulousPath
	tremGppPath	<- get "tremulousgpp"	defaultTremulousGPPPath
	autoMaster	<- get "automaster"	True
	autoClan	<- get "autoclan"	True
	autoGeometry	<- get "savegeometry"	True
	filterBrowser	<- get "browserfilter"	""
	filterPlayers	<- get "playersfilter"	""
	filterEmpty	<- get "showempty"	True
	browserSort	<- get "browsersort"	3
	playersSort	<- get "playerssort"	0
	browserOrder	<- get "browserorder"	SortAscending
	playersOrder	<- get "playersorder"	SortAscending
	delays		<- get "delays"		defaultDelay
	colors		<- makeColorsFromList <$> get "colors" (TFNone "#000000" : map TFColor ["#d60503", "#25c200", "#eab93d", "#0021fe", "#04c9c9", "#e700d7"] ++ [TFNone "#000000"])
	return Config{..}


get a b = fromMaybe b <$> optionWith mread2 a

-}

mread2 :: (Read a) => String -> SM.Maybe a
mread2 x = case reads x of
	[(a, _)]	-> SM.Just a
	_		-> SM.Nothing



makeColorsFromList :: [e] -> Array Int e
makeColorsFromList = listArray (0,7)

configToFile :: Config -> IO ()
configToFile config = handle err $ do
	file	<- inConfDir "config"
	writeFile file (show config)
	where
	err (_::IOError) = gtkWarn "Error saving config file"


configFromFile :: IO Config
configFromFile = handle err $ do
	file	<- inConfDir "config"
	cont	<- readFile file
	case mread cont of
		Prelude.Nothing -> do
			gtkWarn $ "Errors in " ++ file ++ "!\nUsing default"
			return defaultConfig
		Prelude.Just a -> return a
	where
	err (_::IOError) = return defaultConfig
