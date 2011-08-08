module ServerInfo (newServerInfo) where
import Graphics.UI.Gtk

import Prelude hiding (catch)
import Control.Applicative
import Control.Monad hiding (join)
import Data.Ord
import Data.ByteString.Char8 (unpack)
import Data.List (sortBy)
import System.Process
import System.FilePath

import Network.Tremulous.Protocol
import qualified Network.Tremulous.StrictMaybe as SM
import Network.Tremulous.Polling
import Network.Tremulous.Util

import Types
import Exception2
import STM2
import List2
import Monad2
import TremFormatting
import GtkUtils
import GtkExts
import Constants
import Config
import IndividualServerSettings
import SettingsDialog
import FindPlayers
import ServerBrowser

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

	GenSimple amodel aview <- playerView colors "Aliens" True
	GenSimple hmodel hview <- playerView colors "Humans" True
	GenSimple smodel sview <- playerView colors "Spectators" False
	-- For servers not giving the P CVar:
	GenSimple umodel uview <- playerView colors "Players" True


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
	serverbuttons <- hButtonBoxNew
	buttonBoxSetLayout serverbuttons ButtonboxSpread
	let action lbl icon = do
		b <- buttonNewWithMnemonic lbl
		set b	[ buttonImage :=> imageNewFromStock icon IconSizeButton
			, widgetSensitive := False ]
		boxPackStartDefaults serverbuttons b
		return b

	st 	<- action "_Settings" stockProperties
	refresh	<- action "_Refresh" stockRefresh
	join	<- action"_Join" stockConnect

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



	let updateSettings joining = do
		gs@GameServer{..}	<- atomically $ readTMVar current
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

	let launchWithSettings gs
		| protected gs	= whenM (updateSettings True) (launchTremulous gs)
		| otherwise	= launchTremulous gs


	on join buttonActivated $ withTMVar current launchWithSettings
	on st buttonActivated (updateSettings False >> return ())


	let setF boolJoin gs@GameServer{..} = do
		labelSetMarkup hostnamex $ showHostname colors hostname
		zipWithM_ labelSetMarkup info
			[ show address
			, (proto2string protocol ++ (case gamemod of
					SM.Nothing	-> ""
					SM.Just z	-> " (" ++ (unpack . original) z ++ ")"))
			, (unpack . original) mapname
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

		when boolJoin (launchWithSettings gs)

	let updateF PollResult{..} = withTMVar current $ \GameServer{address} ->
			whenJust (serverByAddress address polled) (setF False)


	on refresh buttonActivated $ withTMVar current $ \x -> do
		set refresh [ widgetSensitive := False ]
		Config {delays} <- atomically $ readTMVar mconfig
		forkIO $ do
			result <- pollOne delays (address x)

			SM.whenJust result $ \new -> do
				pr <- atomically $ do
					pr@PollResult{polled} <- takeTMVar mpolled
					let pr' = pr
						{ polled = replace
							(\old -> address old == address new)
							new polled
						}
					putTMVar mpolled pr'
					return pr'
				(_, fb)	<- atomically (readTMVar mupdate)
				clans		<- atomically (readTMVar mclans)
				postGUISync $ do
					fb clans pr
					setF False new
					browserUpdateOne browserStore new
					playerUpdateOne playerStore new



			postGUISync $
				set refresh [ widgetSensitive := True ]

		return ()

	return (rightpane, updateF, setF)
	where
	validping x		= x > 0 && x < 999
	scoreSort		= sortBy (flip (comparing kills))
	showHostname _ (TI _ "")	= "<i>Invalid name</i>"
	showHostname colors x		= pangoPretty colors x
	maybeQ			= SM.maybe "?" show


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

playerView :: ColorArray -> String -> Bool -> IO (GenSimple ListStore Player)
playerView colors teamName showScore = do
	gen@(GenSimple _ view) <- newGenSimple =<< listStoreNew []
	select <- treeViewGetSelection view
	treeSelectionSetMode select SelectionNone
	addColumn gen teamName True $ \rend item -> do
		cellSetMarkup rend $ pangoPrettyBS colors $ name item
		set rend [cellTextEllipsize := EllipsizeEnd]
	when showScore $ do
		addColumn gen "Score" False $ \rend item -> do
			set rend 	[ cellText	:= show $ kills item
					, cellXAlign	:= 1 ]
		return ()
	addColumn gen "Ping" False $ \rend item -> do
			set rend 	[ cellText	:= show $ ping item
					, cellXAlign	:= 1 ]
	return gen
