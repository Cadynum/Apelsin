module ServerInfo where
import Graphics.UI.Gtk

import Control.Monad hiding (join)
import Data.List (sortBy)
import System.Process

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
	hostnamex `labelSetMarkup` "<b><big>Server</big></b>"
	set hostnamex [
		  labelWrap	:= True
		, labelJustify	:= JustifyCenter
		, labelSelectable := True
		-- failgtk exception..
		--, labelWrapMode := WrapPartialWords
		]
	
	-- Pretty Cvar table
	tbl <- tableNew 5 2 True
	let easyAttach pos lbl  = do
		a <- labelNew (Just lbl)
		b <- labelNew Nothing
		miscSetAlignment a 1 0.5
		miscSetAlignment b 0 0.5
		tableAttach tbl a 0 1 pos (pos+1) [Expand, Fill] [Expand, Fill] 8 2
		tableAttach tbl b 1 2 pos (pos+1) [Expand, Fill] [Expand, Fill] 8 2 
		return b
		
	let mkTable xs = mapM (uncurry easyAttach) (zip (iterate (+1) 0) xs)
	datta <- mkTable ["IP:Port", "Game (mod)", "Map", "Password protected"
			, "Slots (+private)", "Ping (server average)"]
	set (head datta) [ labelSelectable := True ]
	
	
	-- Players
	allplayers <- vBoxNew False 4
	--allplayersscroll <- scrollItV allplayers PolicyNever PolicyAutomatic
		
	alienshumans <- hBoxNew True 4
	
	let playerView x = simpleListView [(x, True, pangoPretty colors . name)
					, ("Score", False, show . kills)
					, ("Ping", False, show . ping)
					]
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
	jimg	<- imageNewFromStock stockConnect IconSizeButton
	rimg	<- imageNewFromStock stockRefresh IconSizeButton
	set join 	[ buttonImage := jimg
			, widgetSensitive := False ]
	set refresh	[ buttonImage := rimg
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


	let launchTremulous = withTMVar current $ \GameServer{..} -> do
		tst <- atomically $ tryTakeTMVar running
		whenJust tst terminateProcess 
		Config {tremPath, tremGppPath} <- atomically $ readTMVar mconfig
		let binary = case gameproto of
			69 -> tremPath
			70 -> tremGppPath
			_  -> ""

		set join [ widgetSensitive := False ]
		(_,_,_,p) <- createProcess $ (shell $ binary ++ " +connect " ++ show address)
			{close_fds = True, std_in = Inherit, std_out = Inherit, std_err = Inherit}
		atomically $ putTMVar running p
		forkIO $ do
			threadDelay 1000000
			postGUISync $ set join [ widgetSensitive := True ]
			return ()
		return ()

	on join buttonActivated launchTremulous
		
	let setF boolJoin gs@GameServer{..} = do
		let (a:b:d:e:f:g:_) = datta
		hostnamex `labelSetMarkup` ("<b><big>" ++ pangoColors colors (boxify $ htmlEscape $ unpackorig hostname) ++ "</big></b>")
		a `labelSetMarkup` (show address)
		b `labelSetMarkup` (proto2string gameproto ++ (case gamemod of
					Nothing	-> ""
					Just z	-> " (" ++ unpackorig z ++ ")"))
		d `labelSetMarkup` (unpackorig mapname)
		e `labelSetMarkup` if protected then "Yes" else "No"
		f `labelSetMarkup` (show slots ++ " (+" ++ show privslots ++ ")")
		labelSetMarkup g $ (show gameping) ++ 
			" (" ++ (show $ intmean $ filter validping $ map ping players) ++ ")"
		
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

		
	let updateF = withTMVar mpolled $ \PollMasters{..} -> do
		withTMVar current $ \GameServer{address} -> do
			case serverByAddress address polled of
				Nothing -> gtkWarn "Server is no longer available"
				Just a	-> setF False a
			
	
	on refresh buttonActivated $ withTMVar current $ \x -> do
		set refresh [ widgetSensitive := False ]
		Config {delays} <- atomically $ readTMVar mconfig
		forkIO $ do
			new <- pollOne delays (address x)
			postGUISync $ do
				whenJust new (setF False)
				set refresh [ widgetSensitive := True ]
		return ()
		
	return (rightpane, updateF, setF)
	where
	validping x = x > 0 && x < 999
	scoreSort = sortBy (\x y -> compare (kills y) (kills x))
		
