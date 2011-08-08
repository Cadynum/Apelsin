module Toolbar(newToolbar, getDNS) where
import Graphics.UI.Gtk

import Control.Applicative
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
import STM2
import Monad2
import Config
import About
import ClanFetcher

newToolbar :: Bundle -> [ClanHook] -> [PolledHook] -> [ClanPolledHook] -> IO HBox
newToolbar bundle@Bundle{..} clanHook polledHook bothHook = do
	pb		<- progressBarNew
	set pb		[ widgetNoShowAll := True ]

	refresh		<- mkToolButton "Refresh all" stockRefresh "Ctrl+R or F5"
	clansync	<- mkToolButton "Sync clans" stockSave "Ctrl+S or F6"
	about		<- mkToolButton "About" stockAbout "F7"

	doSync		<- mLock clansync (newClanSync bundle clanHook bothHook)
	doRefresh	<- mLock refresh (newRefresh bundle polledHook bothHook pb)

	on about buttonActivated (newAbout parent)
	on clansync buttonActivated doSync
	on refresh buttonActivated doRefresh

	toolbar	<- hBoxNew False spacing
	boxPackStartDefaults toolbar refresh
	boxPackStartDefaults toolbar clansync
	boxPackStartDefaults toolbar about

	align <- alignmentNew 0 0 0 0
	set align [ containerChild := toolbar ]

	pbrbox <- hBoxNew False spacing
	set pbrbox [ containerBorderWidth := spacing ]
	boxPackStart pbrbox align PackNatural 0
	boxPackStart pbrbox pb PackGrow 0




	on parent keyPressEvent $  do
		kmod	<- eventModifier
		k	<- map toLower <$> eventKeyName
		case kmod of
			[Control]
				| k == "r" -> liftIO doRefresh		>> return True
				| k == "s" -> liftIO doSync		>> return True
			[]	| k == "f5" -> liftIO doRefresh		>> return True
			[]	| k == "f6" -> liftIO doSync		>> return True
			[]	| k == "f7" -> liftIO (newAbout parent)	>> return True
			_ -> return False

	withTMVar mconfig $ \Config{..} -> do
		when autoMaster doRefresh
		when autoClan doSync

	return pbrbox

mkToolButton :: String -> StockId -> String -> IO Button
mkToolButton lbl icon tip = do
	button <- buttonNewWithLabel lbl
	set button	[ buttonImage		:=> imageNewFromStock icon IconSizeButton
			, buttonRelief		:= ReliefNone
			, buttonFocusOnClick	:= False
			, widgetTooltipText	:= Just tip ]
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
	Config {clanSyncURL} <- atomically $ readTMVar mconfig
	forkIO $ do
		new <- getClanList clanSyncURL
		case new of
			Nothing	-> postGUISync $ gtkError "Unable to download clanlist"
			Just a	-> do
				result <- atomically $ do
					swapTMVar mclans a
					readTMVar mpolled
				postGUISync $ do
					mapM_ ($ a) clanHook
					mapM_ (\f -> f a result) bothHook
		unlock
	return ()

newRefresh :: Bundle -> [PolledHook] -> [ClanPolledHook] -> ProgressBar -> IO () -> IO ()
newRefresh Bundle{..} polledHook bothHook pb unlock = do
	progressBarSetFraction pb 0
	widgetShow pb
	Config {masterServers, delays=delays@Delay{..}} <- atomically $ readTMVar mconfig

	start <- getMicroTime

	-- This is a stupid guess based on that about 110 servers will respond and the master
	-- will take about 200ms to respond
	PollResult { serversResponded } <- atomically $ readTMVar mpolled
	let serversGuess = if serversResponded == 0 then 110 else serversResponded
	let tremTime = (packetDuplication + 1) * packetTimeout
		+ serversGuess * throughputDelay + 200 * 1000

	forkIO $ do
		hosts <- catMaybes <$> mapM
			(\(host, port, proto) -> fmap (`MasterServer` proto) <$> getDNS host (show port))
			masterServers
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

		atomically $ replaceTMVar mpolled result
		killThread pbth
		clans <- atomically $ readTMVar mclans
		postGUISync $ do
			mapM_ ($ result) polledHook
			mapM_ (\f -> f clans result) bothHook
			widgetHide pb
		unlock
	return ()
