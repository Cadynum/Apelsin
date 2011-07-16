module Preferences where
import Graphics.UI.Gtk
import Control.Monad
import Control.Applicative hiding (empty)
import Control.Concurrent.STM
import Data.Char
import Data.Array
import Text.Printf
import Network.Tremulous.Protocol
import System.FilePath

import Types
import Config
import Constants
import GtkUtils
import TremFormatting


--newPreferences :: Bundle -> IO ScrolledWindow
newPreferences Bundle{..} = do
	-- Default filters
	(tbl, [filterBrowser', filterPlayers'], filterEmpty') <-
		configTable ["_Browser:", "Find _players:"]
	filters <- newLabeledFrame "Default filters"
	set filters [ containerChild := tbl]

	-- Tremulous path
	(pathstbl, [tremPath', tremGppPath']) <- pathTable parent ["_Tremulous 1.1:", "Tremulous _GPP:"]
	paths <- newLabeledFrame "Tremulous path or command"
	set paths [ containerChild := pathstbl]

	-- Startup
	startupMaster	<- checkButtonNewWithMnemonic "_Refresh all servers"
	startupClan	<- checkButtonNewWithMnemonic "_Sync clan list"
	startupGeometry	<- checkButtonNewWithMnemonic "Restore _window geometry from previous session"
	
	(startup, startupBox) <- framedVBox ("On startup")
	boxPackStartDefaults startupBox startupMaster	
	boxPackStartDefaults startupBox startupClan
	boxPackStartDefaults startupBox startupGeometry
			

	-- Colors

	(colorTbl, colorList)	<- numberedColors 
	(colors', colorBox)	<- framedVBox "Color theme"
	colorWarning		<- labelNew (Just "Note: Requires a restart to take effect")
	
	miscSetAlignment colorWarning 0 0
	boxPackStart colorBox colorTbl PackNatural 0
	boxPackStart colorBox colorWarning PackNatural 0


	-- Internals
	(itbl, [itimeout, iresend, ibuf]) <- mkInternals 
	(internals, ibox) <- framedVBox "Polling Internals"
	ilbl <- labelNew $ Just "Tip: Hover the cursor over each option for a description"
	miscSetAlignment ilbl 0 0
	--labelSetLineWrap ilbl True
	boxPackStart ibox itbl PackNatural 0
	boxPackStart ibox ilbl PackNatural 0


	-- Apply
	apply	<- buttonNewFromStock stockApply
	bbox	<- hBoxNew False 0
	boxPackStartDefaults bbox apply
	balign	<- alignmentNew 0.5 1 0 0
	set balign [ containerChild := bbox ]

	on apply buttonActivated $ do
		filterBrowser	<- get filterBrowser' entryText
		filterPlayers	<- get filterPlayers' entryText
		filterEmpty	<- get filterEmpty' toggleButtonActive
		tremPath	<- get tremPath' entryText
		tremGppPath	<- get tremGppPath' entryText
		autoMaster	<- get startupMaster toggleButtonActive
		autoClan	<- get startupClan toggleButtonActive
		autoGeometry	<- get startupGeometry toggleButtonActive
		packetTimeout	<- (*1000) <$> spinButtonGetValueAsInt itimeout
		packetDuplication	<- spinButtonGetValueAsInt iresend
		throughputDelay	<- (*1000) <$> spinButtonGetValueAsInt ibuf
		
		rawcolors	<- forM colorList $ \(colb, cb) -> do
					bool <- get cb toggleButtonActive
					(if bool then TFColor else TFNone)
						 . colorToHex <$> colorButtonGetColor colb
		old		<- atomically $ takeTMVar mconfig
		let new		= old {filterBrowser, filterPlayers, autoMaster
				, autoClan, autoGeometry, tremPath, tremGppPath
				, colors = makeColorsFromList rawcolors
				, delays = Delay{..}, filterEmpty}
		atomically $ putTMVar mconfig new
		configToFile new
		return ()
	
	-- Main box

	box <- vBoxNew False spacing
	set box [ containerBorderWidth := spacingBig ]
	boxPackStart box filters PackNatural 0
	boxPackStart box paths PackNatural 0
	boxPackStart box startup PackNatural 0
	boxPackStart box colors' PackNatural 0
	boxPackStart box internals PackNatural 0
	boxPackStart box balign PackGrow 0


	-- Set values from Config
	let updateF = do
		Config {..} 		<- atomically $ readTMVar mconfig
		set filterBrowser'	[ entryText := filterBrowser ]
		set filterPlayers'	[ entryText := filterPlayers ]
		set filterEmpty'	[ toggleButtonActive := filterEmpty]
		set tremPath'		[ entryText := tremPath ]
		set tremGppPath'	[ entryText := tremGppPath ]
		set startupMaster	[ toggleButtonActive := autoMaster ]
		set startupClan		[ toggleButtonActive := autoClan ]
		set startupGeometry	[ toggleButtonActive := autoGeometry ]
		set itimeout		[ spinButtonValue := fromIntegral (packetTimeout delays `div` 1000) ]
		set iresend		[ spinButtonValue := fromIntegral (packetDuplication delays) ]
		set ibuf		[ spinButtonValue := fromIntegral (throughputDelay delays `div` 1000) ]
		sequence_ $ zipWith f colorList (elems colors)
		where	f (a, b) (TFColor c) = do
				colorButtonSetColor a (hexToColor c)
				toggleButtonSetActive b True
			f (a ,b) (TFNone c) = do
				colorButtonSetColor a (hexToColor c)
				toggleButtonSetActive b False
				-- Apparently this is needed too
				toggleButtonToggled b
	updateF
		
	return box

configTable :: [String] -> IO (Table, [Entry], CheckButton)
configTable ys = do
	tbl <- tableNew 0 0 False
	empty <- checkButtonNewWithMnemonic "_empty"
	let easyAttach pos lbl  = do
		a <- labelNewWithMnemonic lbl
		b <- entryNew
		set a [ labelMnemonicWidget := b ]
		miscSetAlignment a 0 0.5
		tableAttach tbl a 0 1 pos (pos+1) [Fill] [] spacing spacingHalf
		tableAttach tbl b 1 2 pos (pos+1) [Expand, Fill] [] spacing spacingHalf
		when (pos == 0) $ 
			tableAttach tbl empty 2 3 pos (pos+1) [Fill] [] spacing spacingHalf
				
		return b
		
	let mkTable xs = mapM (uncurry easyAttach) (zip [0..] xs)
	rt	<- mkTable ys
	return (tbl, rt, empty)

pathTable :: Window -> [String] -> IO (Table, [Entry])
pathTable parent ys = do
	tbl <- tableNew 0 0 False
	let easyAttach pos lbl  = do
		a <- labelNewWithMnemonic lbl
		(box, ent) <- pathSelectionEntryNew parent
		set a [ labelMnemonicWidget := ent ]
		miscSetAlignment a 0 0.5
		tableAttach tbl a 0 1 pos (pos+1) [Fill] [] spacing spacingHalf
		tableAttach tbl box 1 2 pos (pos+1) [Expand, Fill] [] spacing spacingHalf
		return ent
		
	let mkTable xs = mapM (uncurry easyAttach) (zip [0..] xs)
	rt	<- mkTable ys
	return (tbl, rt)

mkInternals :: IO (Table, [SpinButton])
mkInternals = do
	tbl <- tableNew 0 0 False
	let easyAttach pos (lbl, lblafter, tip)  = do
		a <- labelNewWithMnemonic lbl
		b <- spinButtonNewWithRange 0 10000 1
		c <- labelNew (Just lblafter)
		set a	[ labelMnemonicWidget := b
			, widgetTooltipText := Just tip
			, miscXalign := 0 ]
		set c	[ miscXalign := 0 ]
		tableAttach tbl a 0 1 pos (pos+1) [Fill] [] spacing spacingHalf
		tableAttach tbl b 1 2 pos (pos+1) [Fill] [] spacing spacingHalf
		tableAttach tbl c 2 3 pos (pos+1) [Fill] [] spacing spacingHalf
		return b
		
	let mkTable xs = mapM (uncurry easyAttach) (zip [0..] xs)
	rt <- mkTable	[ ("Respo_nse Timeout:", "ms", "How long Apelsin should wait before sending a new request to a server possibly not responding")
			, ("Maximum packet _duplication:", "times", "Maximum number of extra requests to send beoynd the initial one if a server does not respond" )
			, ("Throughput _limit:", "ms", "Should be set as low as possible as long as pings from \"Refresh all servers\" remains the same as \"Refresh current\"") ]
	return (tbl, rt)


framedVBox :: String -> IO (Frame, VBox)
framedVBox title = do
	box	<- vBoxNew False 0
	frame	<- newLabeledFrame title
	set box [ containerBorderWidth := spacing ]
	set frame	[ containerChild := box ]
	return (frame, box)
	
numberedColors :: IO (Table, [(ColorButton, CheckButton)])
numberedColors = do
	tbl <- tableNew 0 0 False
	let easyAttach pos lbl  = do
		a <- labelNew (Just lbl)
		b <- colorButtonNew
		c <- checkButtonNew
		on c toggled $ do
			bool <- get c toggleButtonActive
			set b [ widgetSensitive := if bool then True else False ]
		miscSetAlignment a 0.5 0
		tableAttach tbl a pos (pos+1) 0 1 [Fill] [] spacingHalf spacingHalf
		tableAttach tbl b pos (pos+1) 1 2 [Fill] [] spacingHalf spacingHalf
		tableAttach tbl c pos (pos+1) 2 3 [] [] spacingHalf spacingHalf
		return (b, c)
	let mkTable xs = mapM (uncurry easyAttach) (zip (iterate (+1) 0) xs)

	xs <- mkTable ["^0", "^1", "^2", "^3", "^4", "^5", "^6", "^7"]
	return (tbl, xs)


-- Gtk fails yet again and doesn't offer something like this by default
pathSelectionEntryNew :: Window -> IO (HBox, Entry)
pathSelectionEntryNew parent = do
	box	<- hBoxNew False 0
	img	<- imageNewFromStock stockOpen IconSizeButton
	button	<- buttonNew
	set button [ buttonImage := img ]
	ent	<- entryNew

	boxPackStart box ent PackGrow 0
	boxPackStart box button PackNatural 0
	
		
	on button buttonActivated $ do
		fc	<- fileChooserDialogNew (Just "Select path") (Just parent) FileChooserActionOpen
			[ (stockCancel, ResponseCancel)
			, (stockOpen, ResponseAccept) ]
		current <- takeDirectory <$> get ent entryText
		fileChooserSetCurrentFolder fc current
		widgetShow fc
		resp <- dialogRun fc
		case resp of
			ResponseAccept -> do
					tst <- fileChooserGetFilename fc
					whenJust tst $ \path ->
						set ent [ entryText := path ]
			_-> return ()
		widgetDestroy fc
		
	
	return (box, ent)


-- And more fail by gtk to not include something like this by default

colorToHex :: Color -> String
colorToHex (Color a b c) = printf "#%02x%02x%02x" (f a) (f b) (f c)
	where f = (`div` 0x100)


hexToColor :: String -> Color
hexToColor ('#':a:b:c:d:e:g:_)	= Color (f a b) (f c d) (f e g)
	where f x y = fromIntegral $ (digitToInt x * 0x10 + digitToInt y) * 0x100
hexToColor ('#':a:b:c:_)	= Color (f a) (f b) (f c)
	where f x = fromIntegral $ digitToInt x * 0x1000
hexToColor _			= Color 0 0 0
