module Config where

import Tremulous.Protocol
import Control.Exception
import Constants
import TremFormatting
import Helpers
import Data.Array
import GtkUtils

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
	, filterPlayers
	, filterClans	:: !String
	, delays	:: !Delay
	, colors	:: !ColorTheme
	} deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config {
	  configVersion	= 1
	, masterServers = [("master.tremulous.net", 30710, 69), ("master.tremulous.net", 30700, 70)]
	, clanSyncURL	= "http://ddos-tremulous.eu/cw/api/clanlist"
	, tremPath	= "tremulous"
	, tremGppPath	= "tremulous-gpp"
	, autoMaster	= True
	, autoClan	= True
	, autoGeometry	= True
	, filterBrowser	= ""
	, filterPlayers	= ""
	, filterClans	= ""
	, delays	= Delay (400*1000) 2 (2*1000)
	, colors	= makeColorsFromList $
		TFNone : (map TFColor ["#d60503", "#25c200", "#eab93d", "#0021fe", "#04c9c9", "#e700d7"]) ++ [TFNone]

}

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

