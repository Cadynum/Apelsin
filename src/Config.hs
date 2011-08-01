module Config where
import Data.Array
import Control.Exception
import Network.Tremulous.Protocol


import Constants
import List2
import GtkUtils
import TremFormatting


type ColorTheme = Array Int TremFmt

data Config = Config {
	  configVersion	:: !Int
	, masterServers :: ![(String, Int, Int)]
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
	, playersOrder	:: !Bool
	, delays	:: !Delay
	, colors	:: !ColorTheme
	} deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config {
	  configVersion	= 1
	, masterServers = [("master.tremulous.net", 30710, 69), ("master.tremulous.net", 30700, 70)]
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
	, browserOrder	= True
	, playersOrder	= True
	, delays	= defaultDelay
	, colors	= makeColorsFromList $
		TFNone "#000000" : map TFColor ["#d60503", "#25c200", "#eab93d", "#0021fe", "#04c9c9", "#e700d7"] ++ [TFNone "#000000"]
	} 
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
		Nothing -> do
			gtkWarn $ "Errors in " ++ file ++ "!\nUsing default"
			return defaultConfig
		Just a -> return a
	where
	err (_::IOError) = return defaultConfig
