import Graphics.UI.Gtk
import System.Glib.GError

import Control.Applicative
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad
import Data.Maybe
import Network.Socket (withSocketsDo)

import Tremulous.Protocol
import Tremulous.Polling

import STM2
import Constants
import GtkUtils
import ServerInfo
import ServerBrowser
import FindPlayers
import Clanlist
import ClanFetcher
import Preferences
import Config
import Helpers
import About

main :: IO ()
main = withSocketsDo $ do
	unsafeInitGUIForThreadedRTS
	win		<- windowNew
	mvar		<- atomically $ newTMVar []
	config		<- configFromFile	
	mconfig		<- (atomically . newTMVar) config
	mclans		<- (atomically . newTMVar) =<< clanListFromCache

	(currentInfo, currentUpdate, currentSet)<- newServerInfo mvar mconfig
	(browser, browserUpdate)		<- newServerBrowser mconfig currentSet
	(findPlayers, findUpdate)		<- newFindPlayers mvar mconfig (currentSet False)
	(clanlist, clanlistUpdate)		<- newClanList mclans mconfig
	(onlineclans, onlineclansUpdate)	<- newOnlineClans mvar mconfig mclans (currentSet False)
	preferences				<- newPreferences mconfig
	
	-- /// Toolbar /////////////////////////////////////////////////////////////////////////////

	pbrbox <- hBoxNew False g_SPACING
		
	pb		<- progressBarNew
	set pb		[ widgetNoShowAll := True ]
	a_refresh	<- buttonNewWithMnemonic "_Refresh all servers"
	rimg		<- imageNewFromStock stockRefresh IconSizeButton
	set a_refresh	[ buttonImage := rimg 
			, buttonRelief := ReliefNone 
			, buttonFocusOnClick := False ]
	about		<- buttonNewFromStock stockAbout
	set about	[ buttonRelief := ReliefNone 
			, buttonFocusOnClick := False ]

	on about buttonActivated $ newAbout
			
	(clanSync, doSync)	<- newClanSync mconfig mclans (clanlistUpdate >> onlineclansUpdate)	
	
	align		<- alignmentNew 0 0 0 0
	alignbox	<- hBoxNew False g_SPACING
	set align [ containerChild := alignbox ]
	
	boxPackStartDefaults alignbox a_refresh
	boxPackStartDefaults alignbox clanSync
	boxPackStartDefaults alignbox about
			
	
	set pbrbox [ containerBorderWidth := g_SPACING ]
	boxPackStart pbrbox align PackNatural 0
	boxPackStart pbrbox pb PackGrow 0
	
	
	let serverRefresh = do
		progressBarSetFraction pb 0
		widgetShow pb
		a_refresh `set` [ widgetSensitive := False ]
		Config {masterServers, delays=Delay{..}}	<- atomically $ readTMVar mconfig
		hosts <- catMaybes <$> mapM
			(\(host, port, proto) -> fmap (MasterServer proto) <$> getDNS host (show port))
			masterServers
		
		start <- getMicroTime
		let tremTime = (resendTimes + 1) * resendWait
		pbth <- forkIO $ whileTrue $ do
			threadDelay 100000 --10 ms
			now <- getMicroTime
			let diff = now - start
			if now-start > fromIntegral tremTime then do
				progressBarSetFraction pb 1
				return False
			else do
				progressBarSetFraction pb
					(fromIntegral diff / fromIntegral tremTime)
				return True
			
		forkIO $ do
			Config {delays} <- atomically $ readTMVar mconfig
			polled <- pollMasters delays hosts
			-- Force evaluation in this thread to prevent blocking in the mainthread
			polled `deepseq` return ()
			atomically $ clearTMVar mvar >> putTMVar mvar polled
			killThread pbth
			postGUISync $ do
				browserUpdate polled
				findUpdate
				currentUpdate
				onlineclansUpdate
				a_refresh `set` [ widgetSensitive := True ]
				widgetHide pb
		return ()
	on a_refresh buttonActivated serverRefresh
		
	
	-- /// Layout ////////////////////////////////////////////////////////////////////////////

	book <- notebookNew
	notebookAppendMnemonic book browser	"_1: Browser"
	notebookAppendMnemonic book findPlayers	"_2: Find players"
	notebookAppendMnemonic book onlineclans	"_3: Online clans"
	notebookAppendMnemonic book clanlist	"_4: Clan list"
	notebookAppendMnemonic book preferences	"_5: Preferences"
	
	
	leftView <- vBoxNew False 0
	boxPackStart leftView pbrbox PackNatural 0
	boxPackStart leftView book PackGrow 0
	
	
	pane <- hPanedNew
	panedPack1 pane leftView True False
	panedPack2 pane currentInfo  False True
	
	
	-- save the current window size and pane positon on exit
	on win deleteEvent $ tryEvent $ liftIO $ do
		Config {autoGeometry} <- atomically $ readTMVar mconfig
		when autoGeometry $ do
			file		<- inCacheDir "windowsize"
			(winw, winh)	<- windowGetSize win
			ppos		<- panedGetPosition pane
			writeFile file $ show (winw, winh, ppos)
		mainQuit

	handleGError (const $ putStrLn "Window icon not found") $ do
		winicon <- pixbufNewFromFile "icon16.png"
		set win [ windowIcon := Just winicon ]
		
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

	when (autoMaster config) serverRefresh
	when (autoClan config) doSync

	mainGUI
	

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()




