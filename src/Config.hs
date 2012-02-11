module Config (Config(..), ColorTheme, configFromFile, configToFile
	, makeColorsFromList, RefreshMode(..)
) where
import Graphics.UI.Gtk (SortType(..), Window)
import Data.Array
import Data.Char
import Control.Exception
import Control.Applicative
import Network.Tremulous.Protocol
import Control.Monad.State.Strict
import Network.Tremulous.TupleReader (lookupDelete)
import Network.Tremulous.StrictMaybe as SM
import Network.Tremulous.MicroTime

import Constants
import List2
import GtkUtils
import TremFormatting

type ColorTheme = Array Int TremFmt

data SortingOrderPretty = Ascending | Descending
	deriving (Show, Read, Enum)

data RefreshMode = Startup | Auto | Manual
	deriving (Show, Read, Eq)

data Config = Config
	{ masterServers :: ![(String, Int, Int)]
	, clanSyncURL	:: !String
	, tremPath
	, tremGppPath	:: !String
	, refreshMode	:: !RefreshMode
	, autoClan
	, autoGeometry	:: !Bool
	, autoDelay	:: !MicroTime
	, filterBrowser
	, filterPlayers	:: !String
	, filterEmpty	:: !Bool
	, browserSort
	, playersSort
	, clanlistSort	:: !Int
	, browserOrder
	, playersOrder
	, clanlistOrder	:: !SortType
	, delays	:: !Delay
	, colors	:: !ColorTheme
	}


newSave :: Config -> String
newSave Config{delays=Delay{..}, ..} = unlines $
	[ f masterServers	mastersText
	, f clanSyncURL		clanText
	, f tremPath		t11Text
	, f tremGppPath		t12Text
	, f refreshMode		refreshModeText
	, f autoClan		autoClanText
	, f autoGeometry	geometryText
	, f autoDelay		autoDelayText
	, f filterBrowser	browserfilterText
	, f filterPlayers	playerfilterText
	, f filterEmpty		showEmptyText
	, f browserSort		browserSortText
	, f playersSort		playerSortText
	, f clanlistSort	clanlistSortText
	, f (conv browserOrder)	browserOrderText
	, f (conv playersOrder)	playerOrderText
	, f (conv clanlistOrder) clanlistOrderText
	, f packetTimeout	packetTimeoutText
	, f packetDuplication	packetDuplicationText
	, f throughputDelay	throughputDelayText
	] ++
	zipWith (\a b -> f b (colorText ++ [a])) ['0'..'7'] (elems colors)
	where
	conv :: SortType -> SortingOrderPretty
	conv = toEnum . fromEnum
	f :: Show v => v -> String -> String
	f v k = k ++ " " ++ show v

newParse :: [(String, String)] -> Config
newParse = evalState $ do
	masterServers	<- f mastersText		[("master.tremulous.net", 30710, 69), ("master.tremulous.net", 30700, 70)]
	clanSyncURL	<- f clanText			"http://ddos-tremulous.eu/cw/api/clanlist"
	tremPath	<- f t11Text			defaultTremulousPath
	tremGppPath	<- f t12Text			defaultTremulousGPPPath
	refreshMode	<- f refreshModeText		Startup
	autoClan	<- f autoClanText		True
	autoGeometry	<- f geometryText		True
	autoDelay	<- f autoDelayText		1200000000 -- 120s
	filterBrowser	<- f browserfilterText		""
	filterPlayers	<- f playerfilterText		""
	filterEmpty	<- f showEmptyText		True
	browserSort	<- f browserSortText		3
	playersSort	<- f playerSortText		0
	clanlistSort	<- f clanlistSortText		1
	browserOrder	<- toEnum . fromEnum <$> f browserOrderText	Ascending
	playersOrder	<- toEnum . fromEnum <$> f playerOrderText	Ascending
	clanlistOrder	<- toEnum . fromEnum <$> f clanlistOrderText	Ascending
	packetTimeout	<- f packetTimeoutText	(packetTimeout defaultDelay)
	packetDuplication<- f packetDuplicationText	(packetDuplication defaultDelay)
	throughputDelay	<- f throughputDelayText	(throughputDelay defaultDelay)
	colors		<- makeColorsFromList <$> zipWithM (\a b -> f (colorText ++ [a]) b) ['0'..'7'] defaultColors
	return Config{delays = Delay{..}, ..}
	where
	f :: Read b  => String -> b -> State [(String, String)] b
	f key d = do
		s <- get
		let (e, s') = lookupDelete key s
		put s'
		return $ SM.fromMaybe d $ smread =<< e
	defaultColors = TFNone "#000000" : map TFColor ["#d60503", "#25c200", "#eab93d", "#0021fe", "#04c9c9", "#e700d7"] ++ [TFNone "#000000"]

smread :: (Read a) => String -> SM.Maybe a
smread x = case reads x of
	[(a, _)]	-> SM.Just a
	_		-> SM.Nothing

parse :: String -> Config
parse = newParse . map (breakDrop isSpace) . lines

mastersText, clanText, t11Text, t12Text, refreshModeText, autoClanText, geometryText, autoDelayText
	, browserfilterText, playerfilterText, showEmptyText, browserSortText, playerSortText
	, browserOrderText, playerOrderText, packetTimeoutText, clanlistSortText, clanlistOrderText
	, packetDuplicationText, throughputDelayText, colorText :: String
mastersText		= "masters"
clanText		= "clanlistUrl"
t11Text			= "tremulous1.1"
t12Text			= "tremulousGPP"
refreshModeText		= "refreshMode"
autoClanText		= "autoClan"
geometryText		= "saveGeometry"
autoDelayText		= "autoDelay"
browserfilterText	= "browserFilter"
playerfilterText	= "playersFilter"
showEmptyText		= "showEmpty"
browserSortText		= "browserSortColumn"
playerSortText		= "playerSortColumn"
clanlistSortText	= "clanlistSortColumn"
browserOrderText	= "browserSortingOrder"
playerOrderText		= "playerSortingOrder"
clanlistOrderText	= "clanlistSortingOrder"
packetTimeoutText	= "packetTimeout"
packetDuplicationText	= "packetDuplication"
throughputDelayText	= "throughputDelay"
colorText		= "color"

makeColorsFromList :: [e] -> Array Int e
makeColorsFromList = listArray (0,7)

configToFile :: Window -> Config -> IO ()
configToFile win config = do
	file <- inConfDir "config"
	handle err $ do
	writeFile file (newSave config)
	where
	err (e::IOError) = gtkWarn win $ "Unable to save settings:\n" ++ show e


configFromFile :: IO Config
configFromFile = handle err $ do
	file	<- inConfDir "config"
	cont	<- readFile file
	return $ parse cont
	where
	err (_::IOError) = return $ parse ""
