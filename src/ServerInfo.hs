module ServerInfo (newServerInfo) where
import Graphics.UI.Gtk

import Prelude hiding (catch)
import Control.Applicative
import Control.Monad hiding (join)
import Data.Ord
import Data.List (sortBy, findIndex)
import System.Process
import System.FilePath

import Network.Tremulous.Protocol
import Network.Tremulous.Polling
import Network.Tremulous.Util

import Types
import Exception2
import STM2
import List2
import Monad2
import TremFormatting
import GtkUtils
import Constants
import Config
import IndividualServerSettings
import SettingsDialog

newServerInfo :: Bundle -> TMVar (PolledHook, ClanPolledHook) -> IO (VBox, PolledHook, SetCurrent)
newServerInfo Bundle{..} mupdate = do
	Config {colors} <- atomically $ readTMVar mconfig
	current		<- atomically newEmptyTMVar
	running		<- newEmptyMVar
	
	-- Host name
	hostnamex <- labelNew (Just "Server")
	set hostnamex [
		  labelWrap		:= True
		, labelJustify		:= JustifyCenter
		, labelSelectable	:= True
		, labelUseMarkup	:= True
		, labelAttributes 	:= [AttrWeight 0 (-1) WeightBold, AttrScale 0 (-1) 1.2]
		]
	labelSetLineWrapMode hostnamex WrapPartialWords
	
	-- Pretty CVar table
	tbl <- tableNew 0 0 True
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

	bools <- labelNew Nothing
	
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
	serverbuttons <- hBoxNew False 0
	let action lbl icon = do
		b <- buttonNewWithMnemonic lbl
		set b	[ buttonImage :=> imageNewFromStock icon IconSizeButton
			, widgetSensitive := False ]
		boxPackStart serverbuttons b PackRepel 0
		return b
		
	st 	<- action "_Settings" stockProperties
	refresh	<- action "_Refresh" stockRefresh
	join	<- action"_Connect" stockConnect
	
	-- Packing
	rightpane <- vBoxNew False spacing
	set rightpane  [ containerBorderWidth := spacing ]
	boxPackStart rightpane hostnamex PackNatural 1
	boxPackStart rightpane tbl PackNatural 0
	boxPackStart rightpane bools PackNatural 0
	boxPackStart rightpane allplayers PackGrow 0
	boxPackStart rightpane uscroll PackGrow 0
	boxPackStart rightpane serverbuttons PackNatural spacingHalf


	let launchTremulous gs = whenM (isEmptyMVar running) $ do
		putMVar running ()
		config	<- atomically $ readTMVar mconfig
		ss	<- atomically $ readTMVar msettings
		
		set join [ widgetSensitive := False ]
		
		pid <- maybeIO $ runTremulous config gs (getSettings (address gs) ss)
		case pid of
			Nothing -> do
				gtkError $ "Unable to run \"" ++ path ++ "\".\nHave you set your path correctly in Preferences?"
				set join [ widgetSensitive := True ]
				takeMVar running
				where path = case protocol gs of
						70 -> tremGppPath config
						_  -> tremPath config
			Just a -> do
				forkIO $ do
					waitForProcess a
					postGUISync $ set join [ widgetSensitive := True ]
					takeMVar running
				return ()
		
	return ()

	on join buttonActivated $ withTMVar current launchTremulous

	let updateSettings joining = do
		gs@GameServer{..}	<- atomically $readTMVar current
		ss			<- atomically $ readTMVar msettings
		let cur = getSettings address ss
		if not joining || null (serverPass cur) then do
			new <- newSettingsDialog parent colors protected gs cur
			case new of
				Nothing -> return False
				Just n -> do
					let ss' = putSettings address n ss
					atomically $ swapTMVar msettings ss'
					unlessM (toFile ss') $
						gtkWarn "Unable to save server specific settings"
					return True
		else return False
	
	on st buttonActivated (updateSettings False >> return ())

	
	let setF boolJoin gs@GameServer{..} = do
		labelSetMarkup hostnamex $ showHostname colors hostname
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
		labelSetMarkup bools $	unwords	[ if unlagged then "unlagged" else ""
						, if protected then "password" else "" ]
		
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
		
		whenM (isEmptyMVar running) $
			set join [ widgetSensitive := True ]
		set refresh [ widgetSensitive := True ]
		set st [ widgetSensitive := True ]

		
		when boolJoin $ if protected
				then whenM (updateSettings True) (launchTremulous gs)
				else launchTremulous gs
			
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
	showHostname _ (TI _ "")	= "<i>Invalid name</i>"
	showHostname colors x		= pangoPretty colors x
	maybeQ			= maybe "?" show

runTremulous :: Config -> GameServer -> ServerArg-> IO (Maybe ProcessHandle)
runTremulous Config{..} GameServer{..} ServerArg{..} = do
	(_,_,_,p) <- createProcess (proc com args) {cwd = ldir, close_fds = True}
	maybe (Just p) (const Nothing) <$> getProcessExitCode p
	where
	(com, args) = case protocol of
		70 -> (tremGppPath, argsR False)
		_  -> (tremPath, argsR True)

	argsR c = concatMap (uncurry (arg c))
			[ ("connect", show address)
			, ("password", serverPass)
			, ("rconPassword", serverRcon)
			, ("name", serverName)
			] 
	
	ldir = case takeDirectory com of
		"" -> Nothing
		x  -> Just x

-- Tremulous 1.1 unpatched uses -arg while everything else uses +arg
arg :: Bool -> String -> String -> [String]
arg compat a xs = case xs of
	""            -> []
	s | compat    -> ['+':a, s, '-':a, s]
	  | otherwise -> ['+':a, s]
