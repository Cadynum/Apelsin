import Graphics.UI.Gtk
import System.Glib.GError

import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad
import qualified Data.Set as S
import Network.Socket
import Network.Tremulous.Protocol
import System.FilePath (joinPath)

import Types
import Constants
import GtkUtils
import ServerInfo
import ServerBrowser
import FindPlayers
import Clanlist
import ClanFetcher
import Preferences
import Config
import Toolbar

main :: IO ()
main = withSocketsDo $ do
	unsafeInitGUIForThreadedRTS
	win		<- windowNew
	config		<- configFromFile
	cacheclans	<- clanListFromCache
	bundle		<- atomically $ Bundle
				<$> newTMVar (PollResult [] 0 0 S.empty)
				<*> newTMVar config
				<*> newTMVar cacheclans
				<*> pure win

	(currentInfo, currentUpdate, currentSet)<- newServerInfo bundle
	(browser, browserUpdate)		<- newServerBrowser bundle currentSet
	(findPlayers, findUpdate)		<- newFindPlayers bundle currentSet
	(clanlist, clanlistUpdate)		<- newClanList bundle
	(onlineclans, onlineclansUpdate)	<- newOnlineClans bundle currentSet

	preferences				<- newPreferences bundle

	toolbar <- newToolbar bundle
		(clanlistUpdate >> onlineclansUpdate)
		(browserUpdate >> findUpdate >> currentUpdate >> onlineclansUpdate)

	-- /// Layout ////////////////////////////////////////////////////////////////////////////

	book <- notebookNew
	notebookAppendMnemonic book browser	"_1: Browser"
	notebookAppendMnemonic book findPlayers	"_2: Find players"
	notebookAppendMnemonic book onlineclans	"_3: Online clans"
	notebookAppendMnemonic book clanlist	"_4: Clan list"
	notebookAppendMnemonic book preferences	"_5: Preferences"
	leftView <- vBoxNew False 0
	boxPackStart leftView toolbar PackNatural 0
	boxPackStart leftView book PackGrow 0


	pane <- hPanedNew
	panedPack1 pane leftView True False
	panedPack2 pane currentInfo  False True


	-- save the current window size and pane positon on exit
	on win deleteEvent $ tryEvent $ liftIO $ do
		Config {autoGeometry} <- atomically $ readTMVar (mconfig bundle)
		when autoGeometry $ do
			file		<- inCacheDir "windowsize"
			(winw, winh)	<- windowGetSize win
			ppos		<- panedGetPosition pane
			writeFile file $ show (winw, winh, ppos)
		mainQuit

	ddir <- getDataDir
	handleGError (const $ trace $ "Window icon not found: " ++ ddir) $ do
		let list = map
			(\x -> joinPath [ddir, "icons", "hicolor", x ++ "x" ++ x, "apps", "apelsin.png"])
			["16", "32", "48", "64"]
		icons <- mapM pixbufNewFromFile list
		set win [ windowIconList := icons]
	
	-- Without allowshrink the window may change size back and forth when selecting new servers
	set win [ containerChild	:= pane
		, windowTitle		:= fullProgramName
		, windowAllowShrink	:= True
		]

	--Showing it to get size requests
	widgetShowAll leftView

	-- Restore the window size and pane position
	when (autoGeometry config) $ handle ignoreIOException $ do
		file	<- inCacheDir "windowsize"
		fx	<- readFile file
		whenJust (mread fx) $ \(winw::Int, winh, ppos::Int) -> do
			Requisition  _ minh	<- widgetSizeRequest leftView
			Requisition minw _	<- widgetSizeRequest leftView
			let panebuf = 15 -- size we assume the panehandle is at most
			panedSetPosition pane (if minw+panebuf > winw then ppos+panebuf else ppos)
			set win [ widgetHeightRequest	:= max minh winh
				, widgetWidthRequest	:= max (minw+panebuf) winw ]
	widgetShowAll win



	mainGUI


ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()


