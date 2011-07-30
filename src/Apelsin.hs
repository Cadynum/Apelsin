import Graphics.UI.Gtk
import System.Glib.GError

import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad
import Data.Char (toLower)
import qualified Data.Set as S
import Network.Socket
import Network.Tremulous.Protocol
import System.FilePath (joinPath)

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

main :: IO ()
main = withSocketsDo $ do
	unsafeInitGUIForThreadedRTS
	win		<- windowNew
	config		<- configFromFile
	cacheclans	<- clanListFromCache
	bundle	<-	(do
			mpolled		<- atomically $ newTMVar (PollResult [] 0 0 S.empty)
			mconfig		<- atomically $ newTMVar config
			mclans		<- atomically $ newTMVar cacheclans
			browserStore	<- listStoreNew []
			return Bundle {parent = win, ..}
			)
	mupdate <- atomically newEmptyTMVar
	(currentInfo, currentUpdate, currentSet)<- newServerInfo bundle mupdate
	(browser, browserUpdate, ent0)		<- newServerBrowser bundle currentSet
	(findPlayers, findUpdate, ent1)		<- newFindPlayers bundle currentSet
	(clanlist, clanlistUpdate, ent3)	<- newClanList bundle cacheclans currentSet
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

	on win keyPressEvent $  do
		kmod	<- eventModifier
		k	<- map toLower <$> eventKeyName
		case kmod of
			[Alt]	| Just page <- mread k
				, page >= 1
				, page <= 5
				-> do	liftIO (set book [ notebookPage := page - 1])
					return True
			[Control]
				| k == "l" || k == "f"
				-> do	page <- liftIO (get book notebookPage)
					case page of
						0 -> liftIO $ widgetGrabFocus ent0
						1 -> liftIO $ widgetGrabFocus ent1
						3 -> liftIO $ widgetGrabFocus ent3
						_ -> return ()
					return True
			_ -> return False
			
	widgetGrabFocus ent0
	mainGUI


ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()


