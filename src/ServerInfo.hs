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

newServerInfo :: Bundle -> IO (VBox, IO (), Bool -> GameServer -> IO ())
newServerInfo Bundle{..} = do
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
	info <- mkTable ["IP:Port", "Game (mod)", "Map", "Password protected"
			, "Slots (+private)", "Ping (server average)"]
	set (head info) [ labelSelectable := True ]
	
	
	-- Players
	allplayers	<- vBoxNew False 4
	alienshumans	<- hBoxNew True 4
	
	let playerView x = simpleListView
		[ (x		, True	, pangoPretty colors . name)
		, ("Score"	, False	, show . kills)
		, ("Ping"	, False	, show . ping) ]
	(amodel, aview) <- playerView "Aliens"
	(hmodel, hview) <- playerView "Humans"
	(smodel, sview) <- simpleListView [("Spectators", True, pangoPretty colors . name)
					, ("Ping", False, show . ping)]
	
	ascroll <- scrollIt aview PolicyNever PolicyAutomatic
	hscroll <- scrollIt hview PolicyNever PolicyAutomatic
	boxPackStart alienshumans ascroll PackGrow 0
	boxPackStart alienshumans hscroll PackGrow 0


	boxPackStart allplayers alienshumans PackGrow 0
	specscroll <- scrollIt sview PolicyNever PolicyAutomatic
	boxPackStart allplayers specscroll PackNatural 0

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
	boxPackStart rightpane serverbuttons PackNatural 2


	let launchTremulous = withTMVar current $ \gs -> do
		tst <- atomically $ tryTakeTMVar running
		whenJust tst $ \a ->
			catch (terminateProcess a) (\(_ :: IOError) -> return ())
		config <- atomically $ readTMVar mconfig

		set join [ widgetSensitive := False ]

		pid <- runTremulous config gs
		
		atomically $ putTMVar running pid
		
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
			, if protected then "Yes" else "No"
			, show slots ++ " (+" ++ show privslots ++ ")"
			, show gameping ++
				" (" ++ (show . intmean . filter validping . map ping) players ++ ")"
			]
		hostnamex `labelSetMarkup` showHostname colors hostname
		
		listStoreClear amodel
		listStoreClear hmodel
		listStoreClear smodel
		
		let (s', a', h', _) = partitionTeams (scoreSort players)
		mapM_ (listStoreAppend amodel) a'
		mapM_ (listStoreAppend hmodel) h'
		mapM_ (listStoreAppend smodel) s'
		treeViewColumnsAutosize aview
		treeViewColumnsAutosize hview
		treeViewColumnsAutosize sview
		Requisition _ sReq <- widgetSizeRequest sview
		set specscroll [ widgetHeightRequest := sReq ]
		
		atomically $ clearTMVar current >> putTMVar current gs
		
		set join [ widgetSensitive := True ]
		set refresh [ widgetSensitive := True ]

		when boolJoin launchTremulous
		return ()

	let updateF = withTMVar mpolled $ \PollResult{..} ->
		withTMVar current $ \GameServer{address} ->
			case serverByAddress address polled of
				Nothing -> return ()
				Just a	-> setF False a
			
	
	on refresh buttonActivated $ withTMVar current $ \x -> do
		set refresh [ widgetSensitive := False ]
		Config {delays} <- atomically $ readTMVar mconfig
		forkIO $ do
			result <- pollOne delays (address x)
			postGUISync $ do
				whenJust result $ \new -> do
					atomically $ do
						modifyTMVar mpolled $ \pr@PollResult{polled} -> pr
							{ polled = replace
								(\old -> address old == address new)
								new polled
							}
					setF False new
					mm <- findIndex (\old -> address old == address new) <$>
						listStoreToList browserStore
					whenJust mm $ \i -> do
						listStoreSetValue browserStore i new
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

runTremulous :: Config -> GameServer -> IO ProcessHandle
runTremulous Config{..} GameServer{..} = do
	(_,_,_,p) <- createProcess ((toProc launch) {cwd = ldir})
		{close_fds = True, std_in = Inherit, std_out = Inherit, std_err = Inherit}
	return p

	where
	launch@(com,_) = case protocol of
		70 -> (tremGppPath, ["+connect", show address])
		_  -> (tremPath, ["-connect", show address, "+connect", show address])
		
	ldir = case takeDirectory com of
		"" -> Nothing
		x  -> Just x

	toProc (a, b) = case words a of
		(x:xs)	-> proc x (xs ++ b)
		_	-> proc a b
