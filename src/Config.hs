{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}

module Config(
      Config(..)
    , ColorTheme
    , RefreshMode(..)
    , defaultConfig
    , configFromFile
    , configToFile
    , makeColorsFromList
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
    { masterServers         :: ![(String, Int, Int)]
    , clanlistURL           :: !String
    , tremulousPath
    , tremulousGppPath
    , unvanquishedPath      :: !String
    , refreshMode           :: !RefreshMode
    , autoClan
    , restoreGeometry       :: !Bool
    , autoRefreshDelay      :: !MicroTime
    , filterBrowser
    , filterPlayers         :: !String
    , showEmpty             :: !Bool
    , browserSortColumn
    , playersSortColumn
    , clanlistSortColumn    :: !Int
    , browserOrder
    , playersOrder
    , clanlistOrder         :: !SortType
    , delays                :: !Delay
    , colors                :: !ColorTheme
    }

defaultConfig :: Config
defaultConfig = Config
    { masterServers         = [ ("master.tremulous.net", 30710, 69)
                              , ("master.tremulous.net", 30700, 70)
                              , ("master.tremulous.net", 30700, 71)
                              , ("unvanquished.net", 27950, 86)
                              ]
    , clanlistURL           = "http://ddos-tremulous.eu/cw/api/2/clanlist"

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    , tremulousPath         = "C:\\Program Files\\Tremulous\\tremulous.exe"
    , tremulousGppPath      = "C:\\Program Files\\Tremulous\\tremulous-gpp.exe"
    , unvanquishedPath      = "C:\\Program Files (x86)\\Unvanquished\\daemon.exe"
#else
    , tremulousPath         = "tremulous"
    , tremulousGppPath      = "tremulous-gpp"
    , unvanquishedPath      = "unvanquished"
#endif

    , refreshMode           = Startup
    , autoClan              = True
    , restoreGeometry       = True
    , autoRefreshDelay      = 120000000 -- 120 seconds
    , filterBrowser         = ""
    , filterPlayers         = ""
    , showEmpty             = True
    , browserSortColumn     = 3
    , playersSortColumn     = 0
    , clanlistSortColumn    = 1
    , browserOrder          = ascending
    , playersOrder          = ascending
    , clanlistOrder         = ascending
    , delays                = defaultDelay
    , colors                = makeColorsFromList
                                [ TremFmt False "#000000"
                                , TremFmt True  "#d60503"
                                , TremFmt True  "#25c200"
                                , TremFmt True  "#eab93d"
                                , TremFmt True  "#0021fe"
                                , TremFmt True  "#04c9c9"
                                , TremFmt True  "#e700d7"
                                , TremFmt False "#000000"
                                ]
    }
    where ascending = (toEnum . fromEnum) Ascending


#define NS(x) (showKV x #x)
#define NSF(x, y) (showKV ((y) x) #x)
newSave :: Config -> String
newSave Config{delays=Delay{..}, ..} = unlines $
    [ NS(masterServers)
    , NS(clanlistURL)
    , NS(tremulousPath)
    , NS(tremulousGppPath)
    , NS(unvanquishedPath)
    , NS(refreshMode)
    , NS(autoClan)
    , NS(restoreGeometry)
    , NSF(autoRefreshDelay, (`quot`1000000))
    , NS(filterBrowser)
    , NS(filterPlayers)
    , NS(showEmpty)
    , NS(browserSortColumn)
    , NS(playersSortColumn)
    , NS(clanlistSortColumn)
    , NSF(browserOrder, conv)
    , NSF(playersOrder, conv)
    , NSF(clanlistOrder, conv)
    , NS(packetTimeout)
    , NS(packetDuplication)
    , NS(throughputDelay)
    ] ++ zipWith
        (\a b -> showKV b ("color" ++ [a])) ['0'..'7']
        (elems colors)
    where
    conv :: SortType -> SortingOrderPretty
    conv = toEnum . fromEnum
    showKV :: Show v => v -> String -> String
    showKV v k = k ++ " " ++ show v

#define NP(x) x <- getKV #x id (x defaultConfig)
#define NP_NODEF(x) x <- getKV #x id
#define NPF(x, f) x <- getKV #x (f) (x defaultConfig)
newParse :: [(String, String)] -> Config
newParse = evalState $ do
    NP(masterServers)
    NP(clanlistURL)
    NP(tremulousPath)
    NP(tremulousGppPath)
    NP(unvanquishedPath)
    NP(refreshMode)
    NP(autoClan)
    NP(restoreGeometry)
    NPF(autoRefreshDelay, (*1000000))
    NP(filterBrowser)
    NP(filterPlayers)
    NP(showEmpty)
    NP(browserSortColumn)
    NP(playersSortColumn)
    NP(clanlistSortColumn)
    NPF(browserOrder , conv)
    NPF(playersOrder , conv)
    NPF(clanlistOrder, conv)
    NP_NODEF(packetTimeout) (packetTimeout defaultDelay)
    NP_NODEF(packetDuplication) (packetDuplication defaultDelay)
    NP_NODEF(throughputDelay) (throughputDelay defaultDelay)
    colors <- makeColorsFromList <$> zipWithM
        (\a b -> getKV ("color" ++ [a]) id b)
        ['0'..'7']
        (elems (colors defaultConfig))
    return Config{delays = Delay{..}, ..}
    where
    conv :: SortingOrderPretty -> SortType
    conv = toEnum . fromEnum
    getKV :: Read b  => String -> (b -> c) -> c -> State [(String, String)] c
    getKV key f def = do
        s <- get
        let (e, sNew) = lookupDelete key s
        put sNew
        return $ SM.maybe def f $ smread =<< e


smread :: (Read a) => String -> SM.Maybe a
smread x = case reads x of
    [(a, _)]    -> SM.Just a
    _       -> SM.Nothing

makeColorsFromList :: [e] -> Array Int e
makeColorsFromList = listArray (0,7)

parse :: String -> Config
parse = newParse . map (breakDrop isSpace) . lines


configToFile :: Window -> Config -> IO ()
configToFile win config = do
    file <- inConfDir "config"
    handle err $ writeFile file (newSave config)
    where
    err (e::IOError) = gtkWarn win $ "Unable to save settings:\n" ++ show e

configFromFile :: IO Config
configFromFile = catch
    (parse <$> (readFile =<< inConfDir "config"))
    (\(_::IOError) -> return defaultConfig)
