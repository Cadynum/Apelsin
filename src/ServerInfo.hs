module ServerInfo (newServerInfo) where
import Graphics.UI.Gtk

import Prelude hiding (catch)
import Control.Applicative
import Control.Monad hiding (join)
import Control.Exception
import Data.Ord
import Data.List (sortBy, findIndex)
import System.Process
import System.FilePath

import Network.Tremulous.Protocol
import Network.Tremulous.Polling
import Network.Tremulous.Util

import Types
import STM2
import List2
import TremFormatting
import GtkUtils
import Constants
import Config

newServerInfo :: Bundle -> TMVar (PolledHook, ClanPolledHook) -> IO (VBox, PolledHook, SetCurrent)
newServerInfo Bundle{..} mupdate = do
	Config {colors} <- atomically $ readTMVar mconfig
	current		<- atomically newEmptyTMVar
	running		<- atomically newEmptyTMVar
	
	-- Host name
	hostnamex <- labelNew Nothing
	set hostnamex [
		  labelWrap		:= True
		, labelJustify		:= JustifyCenter
		, labelSelectable	:= True
		, labelUseMarkup	:= True
		, labelLabel		:= formatHostname "Server"
		-- failgtk exception..
		--, labelWrapMode := WrapPartialWords
		]
	
	-- Pretty CVar table
	tbl <- tableNew 5 2 True
	set tbl [ tableRowSpacing := spacing
		, tableColumnSpacing := spacingBig ]
	let easyAttach pos lbl  = do
		a <- labelNew (Just lbl)
		b <- labelNew Nothing
		set a [ miscXalign := 1 ]
		set b [ miscXalign := 0 ]
		tableAttachDefaults tbl a 0 1 pos (pos+1)
		tableAttachDefaults tbl b 1 2 pos (pos+1)
		return b
		
	let mkTable = zipWithM easyAttach [0..]
	info <- mkTable ["IP:Port", "Game (mod)", "Map", "Timelimit (SD)"
			, "Slots (+private)", "Ping (server average)"]
	set (head info) [ labelSelectable := True ]
	
	-- Players
	allplayers	<- vBoxNew False spacing
	alienshumans	<- hBoxNew True spacing
	
	let playerView x = simpleListView
		[ (x		, True	, pangoPretty colors . name)
		, ("Score"	, False	, show . kills)
		, ("Ping"	, False	, show . ping) ]
	(amodel, aview) <- playerView "Aliens"
	(hmodel, hview) <- playerView "Humans"
	(smodel, sview) <- simpleListView [("Spectators", True, pangoPretty colors . name)
					, ("Ping", False, show . ping)]
	-- For servers not giving the P CVar
	(umodel, uview) <- playerView "Players"
	
	
	ascroll <- scrollIt aview PolicyNever PolicyAutomatic
	hscroll <- scrollIt hview PolicyNever PolicyAutomatic
	uscroll	<- scrollIt uview PolicyNever PolicyAutomatic
	sscroll <- scrollIt sview PolicyNever PolicyAutomatic
	set uscroll [ widgetNoShowAll := True ]
	boxPackStart alienshumans ascroll PackGrow 0
	boxPackStart alienshumans hscroll PackGrow 0


	boxPackStart allplayers alienshumans PackGrow 0
	boxPackStart allplayers sscroll PackNatural 0

	-- Action buttons
	join	<- buttonNewWithMnemonic "_Join Server"
	refresh	<- buttonNewWithMnemonic "Refresh _current"
	set join 	[ buttonImage :=> imageNewFromStock stockConnect IconSizeButton
			, widgetSensitive := False ]
	set refresh	[ buttonImage :=> imageNewFromStock stockRefresh IconSizeButton
			, widgetSensitive := False ]
	
	serverbuttons <- hBoxNew False 0
	boxPackStart serverbuttons join PackRepel 0
	boxPackStart serverbuttons refresh PackRepel 0
	
	-- Packing
	rightpane <- vBoxNew False spacing
	set rightpane  [ containerBorderWidth := spacing ]
	boxPackStart rightpane hostnamex PackNatural 1
	boxPackStart rightpane tbl PackNatural 0
	boxPackStart rightpane allplayers PackGrow 0
	boxPackStart rightpane uscroll PackGrow 0
	boxPackStart rightpane serverbuttons PackNatural 2


	let launchTremulous = withTMVar current $ \gs -> do
		tst <- atomically $ tryTakeTMVar running
		whenJust tst (ignoreIOException . terminateProcess)
		config <- atomically $ readTMVar mconfig

		set join [ widgetSensitive := False ]

		pid <- maybeIO (runTremulous config gs)
		case pid of
			Nothing -> gtkError $ "Unable to run \"" ++ path ++ "\".\nHave you set your path correctly in Preferences?"
				where path = case protocol gs of
						70 -> tremGppPath config
						_  -> tremPath config
			Just a -> (atomically . putTMVar running) a
		
		forkIO $ do
			threadDelay 1000000
			postGUISync $ set join [ widgetSensitive := True ]
			return ()
		return ()

	on join buttonActivated launchTremulous

	
	let setF boolJoin gs@GameServer{..} = do
		zipWithM_ labelSetMarkup info
			[ show address
			, (proto2string protocol ++ (case gamemod of
					Nothing	-> ""
					Just z	-> " (" ++ unpackorig z ++ ")"))
			, unpackorig mapname
			, maybeQ timelimit ++ " (" ++ maybeQ suddendeath ++ ")"
			, show slots ++ " (+" ++ show privslots ++ ")"
			, show gameping ++
				" (" ++ (show . intmean . filter validping . map ping) players ++ ")"
			]
		hostnamex `labelSetMarkup` showHostname colors hostname
		
		listStoreClear amodel
		listStoreClear hmodel
		listStoreClear smodel
		listStoreClear umodel
		
		let	sortedPlayers		= scoreSort players
			(s', a', h', u')	= partitionTeams sortedPlayers
		if null u' then do		
			mapM_ (listStoreAppend amodel) a'
			mapM_ (listStoreAppend hmodel) h'
			mapM_ (listStoreAppend smodel) s'
			treeViewColumnsAutosize aview
			treeViewColumnsAutosize hview
			treeViewColumnsAutosize sview
			Requisition _ sReq	<- widgetSizeRequest sview
			
			set sscroll [ widgetHeightRequest := min 300 sReq ]
			widgetShow allplayers				
			widgetHide uscroll	
		else do
			mapM_ (listStoreAppend umodel) sortedPlayers
			treeViewColumnsAutosize uview
			widgetShow uscroll
			widgetShow uview
			widgetHide allplayers
		
		atomically $ replaceTMVar current gs
		
		set join [ widgetSensitive := True ]
		set refresh [ widgetSensitive := True ]

		when boolJoin launchTremulous
		return ()

	let updateF PollResult{..} = withTMVar current $ \GameServer{address} ->
			whenJust (serverByAddress address polled) (setF False)
			
	
	on refresh buttonActivated $ withTMVar current $ \x -> do
		set refresh [ widgetSensitive := False ]
		Config {delays} <- atomically $ readTMVar mconfig
		forkIO $ do
			result <- pollOne delays (address x)
			
			whenJust result $ \new -> do
				pr <- atomically $ do
					pr@PollResult{polled} <- takeTMVar mpolled
					let pr' = pr 
						{ polled = replace
							(\old -> address old == address new)
							new polled
						}
					putTMVar mpolled pr'
					return pr'
						
				mm <- findIndex (\old -> address old == address new) <$>
					listStoreToList browserStore
				(fa, fb)	<- atomically (readTMVar mupdate)
				clans		<- atomically (readTMVar mclans)
				postGUISync $ do
					fa pr
					fb clans pr
					setF False new
					-- This generates a gtk assertion fail. Howerver it
					-- seems innocent
					whenJust mm $ \i -> 
						listStoreSetValue browserStore i new	
			postGUISync $
				set refresh [ widgetSensitive := True ]
				
		return ()
		
	return (rightpane, updateF, setF)
	where
	validping x		= x > 0 && x < 999
	scoreSort		= sortBy (flip (comparing kills))
	formatHostname x	= "<b><big>" ++ x ++ "</big></b>"
	showHostname colors x	= formatHostname $ case pangoPretty colors x of
					"" -> "<i>Invalid name</i>"
					a  -> a
	maybeQ			= maybe "?" show

runTremulous :: Config -> GameServer -> IO (Maybe ProcessHandle)
runTremulous Config{..} GameServer{..} = do
	(_,_,_,p) <- createProcess ((proc com args) {cwd = ldir})
		{close_fds = True, std_in = Inherit, std_out = Inherit, std_err = Inherit}
	maybe (Just p) (const Nothing) <$> getProcessExitCode p
	where
	(com, args) = case protocol of
		70 -> (tremGppPath, ["+connect", show address])
		_  -> (tremPath, ["-connect", show address, "+connect", show address])
		
	ldir = case takeDirectory com of
		"" -> Nothing
		x  -> Just x


ignoreIOException :: IO () -> IO ()
ignoreIOException = handle (\(_ :: IOError) -> return ())

maybeIO :: IO (Maybe a) -> IO (Maybe a)
maybeIO = handle (\(_ :: IOError) -> return Nothing)
