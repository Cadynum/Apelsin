import Graphics.UI.Gtk
import System.Glib.GError

import Control.Monad.IO.Class
import Control.Monad
import Data.Char (ord)
import Network.Socket
import Network.Tremulous.Protocol
import Control.Concurrent
import System.FilePath (joinPath)

import Exception2
import Monad2
import List2
import Types
import Constants
import ServerInfo
import ServerBrowser
import FindPlayers
import Clanlist
import OnlineClans
import ClanFetcher
import Preferences
import Config
import Toolbar
import IndividualServerSettings as ISS

main :: IO ()
main = withSocketsDo $ do
	unsafeInitGUIForThreadedRTS
	win		<- windowNew
	config		<- configFromFile
	cacheclans	<- clanListFromCache
	settings	<- ISS.fromFile

	bundle	<-	(do
			mpolled		<- atomically $ newTMVar (PollResult [] 0 0)
			mconfig		<- atomically $ newTMVar config
			mclans		<- atomically $ newTMVar cacheclans
			mrefresh	<- newEmptyMVar
			browserStore	<- listStoreNew []
			playerStore	<- listStoreNew []
			msettings	<- atomically $ newTMVar settings
			return Bundle {parent = win, ..}
			)
	putMVar (mrefresh bundle) ()
	forkIO $ threadDelay (1000*60*1000) >> takeMVar (mrefresh bundle)


	mupdate <- atomically newEmptyTMVar
	(currentInfo, currentUpdate, currentSet)<- newServerInfo bundle mupdate
	(browser, browserUpdate)		<- newServerBrowser bundle currentSet
	(findPlayers, findUpdate)		<- newFindPlayers bundle currentSet
	(clanlist, clanlistUpdate)		<- newClanList bundle cacheclans currentSet
	(onlineclans, onlineclansUpdate)	<- newOnlineClans bundle currentSet

	preferences				<- newPreferences bundle
	atomically $ putTMVar mupdate (findUpdate, onlineclansUpdate)

	toolbar <- newToolbar bundle
		[]
		[browserUpdate, findUpdate, currentUpdate]
		[onlineclansUpdate, clanlistUpdate]

	-- /// Layout ////////////////////////////////////////////////////////////////////////////

	book <- notebookNew
	notebookAppendPage book browser		"Browser"
	notebookAppendPage book findPlayers	"Find players"
	notebookAppendPage book onlineclans	"Online clans"
	notebookAppendPage book clanlist	"Clan list"
	notebookAppendPage book preferences	"Preferences"
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
	handleGError (const $ trace $ "Window icon not found in: " ++ ddir) $ do
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
	when (autoGeometry config) $ ignoreIOException $ do
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

	on win keyPressEvent $ do
		kmod	<- eventModifier
		k	<- eventKeyVal
		let page = fromIntegral k - ord '0' - 1
		if kmod == [Alt] && page >= 0 && page <= 5
			then liftIO (notebookSetCurrentPage book page) >> return True
			else return False
	mainGUI
