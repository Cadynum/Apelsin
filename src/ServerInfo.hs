module ServerInfo (newServerInfo) where
import Graphics.UI.Gtk

import Prelude hiding (catch)
import Control.Applicative
import Control.Monad hiding (join)
import Control.Exception
import Control.Concurrent
import Data.Ord
import Data.ByteString.Char8 (unpack)
import Data.List (sortBy)
import System.Process
import System.FilePath

import Network.Tremulous.Protocol
import qualified Network.Tremulous.StrictMaybe as SM
import Network.Tremulous.Polling
import Network.Tremulous.Util

import ConcurrentUtil
import Types
import Exception2
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
import AutoRefresh

newServerInfo :: Bundle -> MVar (PolledHook, ClanPolledHook) -> IO (VBox, PolledHook, SetCurrent)
newServerInfo Bundle{..} mupdate = do
	Config {colors} <- readMVar mconfig
	current		<- newEmptyMVar
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
		, tableColumnSpacing := spacing ]
	let easyAttach pos lbl  = do
		a <- labelNew (Just lbl)
		b <- labelNew Nothing
		let h = 0x8888
		labelSetAttributes a [AttrForeground 0 (-1) (Color h h h)]

		set a [ miscXalign := 1 ]
		set b [ miscXalign := 0, labelSelectable := True]
		tableAttachDefaults tbl a 0 1 pos (pos+1)
		tableAttachDefaults tbl b 1 2 pos (pos+1)
		return b

	let mkTable = zipWithM easyAttach [0..]
	info <- mkTable ["Address", "Game", "Map", "Timelimit / SD"
			, "Slots (+private)", "Ping / Average"]
	versionLabel <- labelNew Nothing
	bools <- labelNew Nothing

	labelSetSelectable versionLabel True
	labelSetSelectable bools True

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

	Requisition _ natural	<- widgetSizeRequest sscroll
	on allplayers sizeAllocate $ \(Rectangle _ _ _ h') -> do
		let h = h' - spacing
		Requisition _ sReq	<- widgetSizeRequest sview
		Requisition _ aReq	<- widgetSizeRequest aview
		Requisition _ hReq	<- widgetSizeRequest hview
		let setS play spec = do
			widgetSetSizeRequest ascroll (-1) play
			widgetSetSizeRequest hscroll (-1) play
			widgetSetSizeRequest sscroll (-1) spec
		let pReq = max hReq aReq

		if natural * 2 > h then
			let half = max 0 (h`quot`2) in setS half half
		else if (max sReq natural) + pReq + spacing <= h then
			setS (-1) (max sReq natural)
		else do
			let ratio	= fromIntegral pReq / (fromIntegral sReq + fromIntegral pReq) :: Double
			    pNew	= max natural (round (ratio * fromIntegral h))
			    sNew	= max natural (h - pNew)
			setS pNew sNew

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
	join	<- action "_Join" stockConnect

	-- Packing
	rightpane <- vBoxNew False spacing
	set rightpane  [ containerBorderWidth := spacing ]
	boxPackStart rightpane hostnamex PackNatural 1
	boxPackStart rightpane tbl PackNatural 0
	boxPackStart rightpane versionLabel PackNatural 0
	boxPackStart rightpane bools PackNatural 0
	boxPackStart rightpane allplayers PackGrow 0
	boxPackStart rightpane uscroll PackGrow 0
	boxPackStart rightpane serverbuttons PackNatural spacingHalf


	let launchTremulous gs = whenM (isEmptyMVar running) $ do
		putMVar running ()
		config	<- readMVar mconfig
		ss	<- readMVar msettings

		set join [ widgetSensitive := False ]

		pid <- maybeIO $ runTremulous config gs (getSettings (address gs) ss)
		case pid of
			Nothing -> do
				let msg = case protocolToPath config (protocol gs) of
					Just path -> "Unable to run \"" ++ path ++ "\".\nHave you set your path correctly in Preferences?"
					Nothing   -> "Unknown game (protocol: " ++ show (protocol gs) ++ ")"
				gtkError parent msg
				set join [ widgetSensitive := True ]
				takeMVar running

			Just a -> do
				autoSignal mauto AutoPause
				forkIO $ do
					waitForProcess a
					postGUISync $ set join [ widgetSensitive := True ]
					takeMVar running
					autoSignal mauto AutoResume
				return ()

	let updateSettings joining = do
		gs@GameServer{..}	<- readMVar current
		ss			<- readMVar msettings
		let cur = getSettings address ss
		if not joining || null (serverPass cur) then do
			new <- newSettingsDialog parent colors protected gs cur
			case new of
				Nothing -> return False
				Just n -> do
					let ss' = putSettings address n ss
					swapMVar msettings ss'
					unlessM (toFile ss') $
						gtkWarn parent "Unable to save server specific settings"
					return True
		else return False

	let launchWithSettings gs
		| protected gs	= whenM (updateSettings True) (launchTremulous gs)
		| otherwise	= launchTremulous gs


	on join buttonActivated $ readWithMVar current launchWithSettings
	on st buttonActivated (updateSettings False >> return ())


	let setF boolJoin gs@GameServer{..} = do
		labelSetMarkup hostnamex $ showHostname colors hostname
		zipWithM_ labelSetMarkup info
			[ show address
			, (protoToFull protocol ++ (case gamemod of
					SM.Nothing	-> ""
					SM.Just z	-> " (" ++ (unpack . original) z ++ ")"))
			, maybeS mapname
			, maybeQ timelimit ++ " / " ++ maybeQ suddendeath
			, show slots ++ " (+" ++ maybeQ privslots ++ ")"
			, show gameping ++ " / " ++ meanPing players
			, maybeS version
			]
		labelSetText versionLabel (maybeS version)
		labelSetText bools $ unwords [ if unlagged then "unlagged" else ""
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
			widgetShow allplayers
			widgetHide uscroll
		else do
			mapM_ (listStoreAppend umodel) sortedPlayers
			treeViewColumnsAutosize uview
			widgetShow uscroll
			widgetShow uview
			widgetHide allplayers

		mask_ $ do
			tryTakeMVar current
			putMVar current gs

		whenM (isEmptyMVar running) $
			set join [ widgetSensitive := True ]
		set refresh [ widgetSensitive := True ]
		set st [ widgetSensitive := True ]

		when boolJoin (launchWithSettings gs)

	let updateF PollResult{..} = readWithMVar current $ \GameServer{address} ->
			whenJust (serverByAddress address polled) (setF False)


	on refresh buttonActivated $ readWithMVar current $ \x -> do
		set refresh [ widgetSensitive := False ]
		Config {delays} <- readMVar mconfig
		forkIO $ do
			result <- pollOne delays (address x)

			SM.whenJust result $ \new -> do
				pr <- do
					pr@PollResult{polled} <- takeMVar mpolled
					let pr' = pr
						{ polled = replace
							(\old -> address old == address new)
							new polled
						}
					putMVar mpolled pr'
					return pr'
				(_, fb)	<- readMVar mupdate
				clans	<- readMVar mclans
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
	maybeS			= SM.maybe "" (unpack . original)
	meanPing		= show . intmean . filter validping . map ping


protocolToPath :: Config -> Int -> Maybe FilePath
protocolToPath c proto = case proto of
	69 -> Just (tremulousPath c)
	70 -> Just (tremulousGppPath c)
	86 -> Just (unvanquishedPath c)
	_  -> Nothing


runTremulous :: Config -> GameServer -> ServerArg-> IO (Maybe ProcessHandle)
runTremulous config GameServer{..} ServerArg{..} =
	case protocolToPath config protocol of
		Just com -> do
			(_,_,_,p) <- createProcess (proc com args) {cwd = ldir, close_fds = True}
			maybe (Just p) (const Nothing) <$> getProcessExitCode p
			where
			ldir = case takeDirectory com of
				"" -> Nothing
				x  -> Just x

		Nothing ->
			return Nothing
	where
	args = concatMap (uncurry arg)
			[ ("connect", show address)
			, ("password", serverPass)
			, ("rconPassword", serverRcon)
			, ("name", serverName)
			]



arg :: String -> String -> [String]
arg _ "" = []
arg a s  = ['+':a, s]

playerView :: ColorArray -> String -> Bool -> IO (GenSimple ListStore Player)
playerView colors teamName showScore = do
	gen@(GenSimple _ view) <- newGenSimple =<< listStoreNew []
	select <- treeViewGetSelection view
	treeSelectionSetMode select SelectionNone
	addColumn gen teamName True [cellTextEllipsize := EllipsizeEnd] $ \rend ->
		cellSetMarkup rend . pangoPrettyBS colors . name
	when showScore $ do
		addColumn gen "Score" False [cellXAlign := 1] $ \rend item -> do
			set rend  [cellText := show $ kills item]
		return ()
	addColumn gen "Ping" False [cellXAlign := 1] $ \rend item ->
			set rend [ cellText := show $ ping item]
	return gen
