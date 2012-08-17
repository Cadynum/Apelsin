module Toolbar(newToolbar, getDNS) where
import Graphics.UI.Gtk

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Char (toLower)

import Network.Tremulous.Protocol
import Network.Tremulous.Polling
import Network.Tremulous.MicroTime

import GtkUtils
import Types
import Constants
import Monad2
import Config
import About
import ClanFetcher
import AutoRefresh

newToolbar :: Bundle -> [ClanHook] -> [PolledHook] -> [ClanPolledHook] -> IO HBox
newToolbar bundle@Bundle{..} clanHook polledHook bothHook = do
	c		<- readMVar mconfig
	pb		<- progressBarNew
	set pb		[ widgetNoShowAll := True ]

	refresh		<- mkToolButton "Refresh all" stockRefresh "Ctrl+R or F5"
	clansync	<- mkToolButton "Sync clans" stockSave "Ctrl+S or F6"
	about		<- mkToolButton "About" stockAbout "F7"
	auto		<- mkAuto (refreshMode c)

	doSync		<- mLock clansync (newClanSync bundle clanHook bothHook)
	doRefresh	<- mLock refresh (newRefresh bundle polledHook bothHook pb)
	autoRunner mauto mconfig doRefresh

	on about buttonActivated (newAbout parent)
	on clansync buttonActivated doSync
	on refresh buttonActivated doRefresh

	on auto toggled $ do
		act <- toggleButtonGetActive auto
		autoSignal mauto (if act then AutoStart else AutoStop)

	toolbar	<- hBoxNew False 0 -- should be spacing?
	boxPackStartDefaults toolbar refresh
	boxPackStartDefaults toolbar auto
	boxPackStartDefaults toolbar clansync
	boxPackStartDefaults toolbar about

	align <- alignmentNew 0 0 0 0
	set align [ containerChild := toolbar ]

	pbrbox <- hBoxNew False spacing
	set pbrbox [ containerBorderWidth := spacing ]
	boxPackStart pbrbox align PackNatural 0
	boxPackStart pbrbox pb PackGrow 0

	on parent keyPressEvent $  do
		kmod <- eventModifier
		k <- map toLower <$> eventKeyName
		case kmod of
			[Control]
				| k == "r" -> liftIO doRefresh		>> return True
				| k == "s" -> liftIO doSync		>> return True
			[]	| k == "f5" -> liftIO doRefresh		>> return True
			[]	| k == "f6" -> liftIO doSync		>> return True
			[]	| k == "f7" -> liftIO (newAbout parent)	>> return True
			_ -> return False

	case (refreshMode c) of
		Startup	-> doRefresh
		Auto	-> autoSignal mauto AutoStart
		Manual	-> return ()
	when (autoClan c) doSync

	return pbrbox

-- The reason a hbox is used is so the icons always gets displayed regardless of the gtk setting
mkToolButton :: String -> StockId -> String -> IO Button
mkToolButton text icon tip = do
	button <- buttonNew
	img <- imageNewFromStock icon IconSizeButton
	lbl <- labelNew (Just text)
	box <- hBoxNew False 2
	boxPackStart box img PackNatural 0
	boxPackStart box lbl PackNatural 0
	containerAdd button box
	set button	[ buttonRelief		:= ReliefNone
			, buttonFocusOnClick	:= False
			, widgetTooltipText	:= Just tip ]
	return button

mkAuto :: RefreshMode -> IO ToggleButton
mkAuto refreshMode = do
	button <- toggleButtonNew
	set button [ toggleButtonActive := refreshMode == Auto ]
	img <- imageNewFromStock stockMediaPlay IconSizeButton
	containerAdd button img
	set button	[ buttonRelief		:= ReliefNone
			, buttonFocusOnClick	:= False
			, widgetTooltipText	:= Just "Refresh all servers periodically" ]
	return button

mLock :: WidgetClass w => w -> (IO () -> IO ()) -> IO (IO ())
mLock widget f = do
	m <- newEmptyMVar
	let lock = do
		putMVar m ()
		set widget [ widgetSensitive := False ]
	let unlock = do
		takeMVar m
		postGUISync (set widget [ widgetSensitive := True ])

	return $ whenM (isEmptyMVar m) (lock >> f unlock)


newClanSync :: Bundle -> [ClanHook] -> [ClanPolledHook] -> IO () -> IO ()
newClanSync Bundle{..} clanHook bothHook unlock = do
	Config {clanlistURL} <- readMVar mconfig
	forkIO $ do
		new <- getClanList clanlistURL
		case new of
			Nothing	-> postGUISync $ gtkError parent "Unable to download clanlist"
			Just a	-> do
				swapMVar mclans a
				result <- readMVar mpolled
				postGUISync $ do
					mapM_ ($ a) clanHook
					mapM_ (\f -> f a result) bothHook
		unlock
	return ()

newRefresh :: Bundle -> [PolledHook] -> [ClanPolledHook] -> ProgressBar -> IO () -> IO ()
newRefresh Bundle{..} polledHook bothHook pb unlock = do
	progressBarSetFraction pb 0
	widgetShow pb
	Config {masterServers, delays=delays@Delay{..}} <- readMVar mconfig

	-- This is a stupid guess based on that about 110 servers will respond and the master
	-- will take about 200ms to respond
	PollResult { serversResponded } <- readMVar mpolled
	let serversGuess = if serversResponded == 0 then 110 else serversResponded
	let tremTime = (packetDuplication + 1) * packetTimeout
		+ serversGuess * throughputDelay + 200 * 1000

	forkIO $ do
		hosts <- catMaybes <$> mapM
			(\(host, port, proto) -> fmap (`MasterServer` proto) <$> getDNS host (show port))
			masterServers
		start <- getMicroTime
		pbth <- forkIO $ whileTrue $ do
				threadDelay 10000 --10 ms, 100 fps
				now <- getMicroTime
				let diff = now - start
				if now-start > fromIntegral tremTime then do
					postGUISync $ progressBarSetFraction pb 1
					return False
				else do
					postGUISync $ progressBarSetFraction pb
						(fromIntegral diff / fromIntegral tremTime)
					return True

		result <- pollMasters delays hosts
		swapMVar mpolled result
		killThread pbth
		clans <- readMVar mclans
		autoSignal mauto AutoUpdate
		postGUISync $ do
			mapM_ ($ result) polledHook
			mapM_ (\f -> f clans result) bothHook
			widgetHide pb
		unlock
	return ()
