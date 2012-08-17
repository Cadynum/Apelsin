{-# LANGUAGE CPP #-}
-- This file is horribly coded :( A good example why the gtk code should be separate
module Preferences (newPreferences) where
import Graphics.UI.Gtk

import Control.Monad
import Control.Applicative hiding (empty)
import Data.Char
import Data.Array
import Text.Printf
import Network.Tremulous.Protocol
import System.FilePath

import Types
import qualified Config as C
import Constants
import GtkUtils
import Monad2
import TremFormatting

#define _CONNECT(WID, SIG, GET, SET) on WID SIG (do {new <- get WID GET; update (\x -> x { SET = new})})
#define _CONNECT_WITH(WID, SIG, GET, SET, F) on WID SIG (do {new <- get WID GET; update (\x -> x { SET = F new})})

newPreferences :: Bundle -> IO ScrolledWindow
newPreferences Bundle{..} = do
	let update f = do
		new <- modifyMVar mconfig $ \old -> do
			let new = f old
			return (new, new)
		C.configToFile parent new

	-- Default filters
	(  tbl
	 , [filterBrowser, filterPlayers]
	 , showEmpty
	 ) <- configTable
		[ "_Browser:"
		, "_Find players:"
		]
	filters <- framed "Default filters" tbl

	-- Tremulous path
        path_table      <- paddedTableNew
        tremulousPath        <- pathInsertTable parent path_table 0 "_Tremulous 1.1:"
        tremulousGppPath     <- pathInsertTable parent path_table 1 "Tremulous _GPP:"
        unvanquishedPath         <- pathInsertTable parent path_table 2 "_Unvanquished:"
        paths           <- framed "Path or command" path_table

	-- Startup
	autoClan	<- checkButtonNewWithMnemonic "_Sync clan list"
	restoreGeometry	<- checkButtonNewWithMnemonic "Restore _window geometry from previous session"
	startupBox <- vBoxNew False 0
	boxPackStartDefaults startupBox autoClan
	boxPackStartDefaults startupBox restoreGeometry
	startup <- framed "On startup" startupBox

	--Refresh mode
	rb1 <- radioButtonNewWithMnemonic "_Once at startup"
	rb2 <- radioButtonNewWithMnemonicFromWidget rb1 "_Periodically each"
	rb3 <- radioButtonNewWithMnemonicFromWidget rb1 "_Manually"
	autoRefreshDelay <- spinButtonNewWithRange 0 3600 1
	seconds <- labelNew (Just "s")

	delaybox <- hBoxNew False spacing
	boxPackStart delaybox autoRefreshDelay PackNatural 0
	boxPackStart delaybox seconds PackNatural 0

	perbox <- hBoxNew False spacingBig
	boxPackStart perbox rb2 PackNatural 0
	boxPackStart perbox delaybox PackNatural 0

	rbBox <- vBoxNew False 0
	boxPackStartDefaults rbBox rb1
	boxPackStartDefaults rbBox perbox
	boxPackStartDefaults rbBox rb3
	refreshmode <- framed "Refresh all" rbBox

	-- Colors
	(colorTbl, colorList) <- numberedColors
	colorWarning <- labelNew (Just "Note: Requires a restart to take effect")
	miscSetAlignment colorWarning 0 0
	colorBox <- vBoxNew False spacing
	boxPackStart colorBox colorTbl PackNatural 0
	boxPackStart colorBox colorWarning PackNatural 0
	colors' <- framed "Color theme" colorBox


	-- Internals
	(itbl, [packetTimeout', packetDuplication', throughputDelay']) <- mkInternals
	ilbl <- labelNew $ Just "Tip: Hover the cursor over each option for a description"
	miscSetAlignment ilbl 0 0
	ibox <- vBoxNew False spacing
	boxPackStart ibox itbl PackNatural 0
	boxPackStart ibox ilbl PackNatural 0

	internals <- framed "Polling Internals" ibox

	-- Main box
	box <- vBoxNew False spacingHuge
	containerSetBorderWidth box spacing
	boxPackStart box filters PackNatural 0
	boxPackStart box paths PackNatural 0
	boxPackStart box refreshmode PackNatural 0
	boxPackStart box startup PackNatural 0
	boxPackStart box colors' PackNatural 0
	boxPackStart box internals PackNatural 0


	-- Set values from Config
	let updateF = do
		c <- readMVar mconfig
		let Delay{..} = C.delays c
		set filterBrowser	[ entryText := C.filterBrowser c ]
		set filterPlayers	[ entryText := C.filterPlayers c ]
		set showEmpty		[ toggleButtonActive := C.showEmpty c]
		set tremulousPath	[ entryText := C.tremulousPath c ]
		set tremulousGppPath	[ entryText := C.tremulousGppPath c]
		set unvanquishedPath	[ entryText := C.unvanquishedPath c]
		set autoClan		[ toggleButtonActive := C.autoClan c]
		set restoreGeometry	[ toggleButtonActive := C.restoreGeometry c]
		set autoRefreshDelay	[ spinButtonValue := fromIntegral (C.autoRefreshDelay c `quot` 1000000) ]
		set packetTimeout'	[ spinButtonValue := fromIntegral (packetTimeout `quot` 1000) ]
		set packetDuplication'	[ spinButtonValue := fromIntegral packetDuplication ]
		set throughputDelay'	[ spinButtonValue := fromIntegral (throughputDelay `quot` 1000) ]
		set rb1			[ toggleButtonActive := C.refreshMode c == C.Startup ]
		set rb2			[ toggleButtonActive := C.refreshMode c == C.Auto ]
		set rb3			[ toggleButtonActive := C.refreshMode c == C.Manual ]

		zipWithM_ f colorList (elems (C.colors c))
		where	f (a, b) (TremFmt active color) = do
				colorButtonSetColor a (hexToColor color)
				toggleButtonSetActive b active
				toggleButtonToggled b

	updateF

	_CONNECT(filterBrowser, editableChanged, entryText, C.filterBrowser)
	_CONNECT(showEmpty, toggled, toggleButtonActive, C.showEmpty)
	_CONNECT(filterPlayers, editableChanged, entryText, C.filterPlayers)

	_CONNECT(tremulousPath, editableChanged, entryText, C.tremulousPath)
	_CONNECT(tremulousGppPath, editableChanged, entryText, C.tremulousGppPath)
	_CONNECT(unvanquishedPath, editableChanged, entryText, C.unvanquishedPath)

	_CONNECT(autoClan, toggled, toggleButtonActive, C.autoClan)
	_CONNECT(restoreGeometry, toggled, toggleButtonActive, C.restoreGeometry)


	let radiofunc value = update (\x -> x { C.refreshMode = value})

	on rb1 toggled $ do	b <- toggleButtonGetActive rb1
				when b (radiofunc C.Startup)
	on rb2 toggled $ do	b <- toggleButtonGetActive rb2
				when b (radiofunc C.Auto)
	on rb3 toggled $ do	b <- toggleButtonGetActive rb3
				when b (radiofunc C.Manual)

	onValueSpinned autoRefreshDelay $ do
		value <- (*1000000) <$> spinButtonGetValueAsInt autoRefreshDelay
		update (\x -> x { C.autoRefreshDelay = fromIntegral value})

	onValueSpinned packetTimeout' $ do
		packetTimeout <- (*1000) <$> spinButtonGetValueAsInt packetTimeout'
		update (\x -> let delays = C.delays x in x {C.delays = delays {packetTimeout}})

	onValueSpinned packetDuplication' $ do
		packetDuplication <- spinButtonGetValueAsInt packetDuplication'
		update (\x -> let delays = C.delays x in x {C.delays = delays {packetDuplication}})

	onValueSpinned throughputDelay' $ do
		throughputDelay <- (*1000) <$> spinButtonGetValueAsInt throughputDelay'
		update (\x -> let delays = C.delays x in x {C.delays = delays {throughputDelay}})

	let updateColors = do
		rawcolors <- forM colorList $ \(colb, cb) ->
			TremFmt <$> get cb toggleButtonActive
				<*> (colorToHex <$> colorButtonGetColor colb)
		update $ \x -> x {C.colors = C.makeColorsFromList rawcolors}

	forM colorList $ \(colb, cb) -> do
		on cb toggled updateColors
		afterColorSet colb updateColors

	scrollItV box PolicyNever PolicyAutomatic

configTable :: [String] -> IO (Table, [Entry], CheckButton)
configTable ys = do
	tbl <- paddedTableNew
	empty <- checkButtonNewWithMnemonic "_empty"
	let easyAttach pos lbl  = do
		a <- labelNewWithMnemonic lbl

		ent <- entryNew
		b <- hBoxNew False spacingHalf
		boxPackStart b ent PackGrow 0
		when (pos == 0) $
			boxPackStart b empty PackNatural 0

		set a [ labelMnemonicWidget := ent ]
		miscSetAlignment a 0 0.5
		tableAttach tbl a 0 1 pos (pos+1) [Fill] [] 0 0
		tableAttach tbl b 1 2 pos (pos+1) [Expand, Fill] [] 0 0

		return ent

	rt <- zipWithM easyAttach [0..] ys
	return (tbl, rt, empty)

pathInsertTable :: Window -> Table -> Int -> String -> IO Entry
pathInsertTable parent tbl pos lbl = do
	a <- labelNewWithMnemonic lbl
	(box, ent) <- pathSelectionEntryNew parent
	set a [ labelMnemonicWidget := ent ]
	miscSetAlignment a 0 0.5
	tableAttach tbl a 0 1 pos (pos+1) [Fill] [] 0 0
	tableAttach tbl box 1 2 pos (pos+1) [Expand, Fill] [] 0 0
	return ent

mkInternals :: IO (Table, [SpinButton])
mkInternals = do
	tbl <- paddedTableNew
	let easyAttach pos (lbl, lblafter, tip)  = do
		a <- labelNewWithMnemonic lbl
		b <- spinButtonNewWithRange 0 10000 1
		c <- labelNew (Just lblafter)
		set a	[ labelMnemonicWidget := b
			, widgetTooltipText := Just tip
			, miscXalign := 0 ]
		set c	[ miscXalign := 0 ]
		tableAttach tbl a 0 1 pos (pos+1) [Fill] [] 0 0
		tableAttach tbl b 1 2 pos (pos+1) [Fill] [] 0 0
		tableAttach tbl c 2 3 pos (pos+1) [Fill] [] 0 0
		return b

	let mkTable = zipWithM easyAttach [0..]
	rt <- mkTable	[ ("Respo_nse Timeout:", "ms", "How long Apelsin should wait before sending a new request to a server possibly not responding")
			, ("Maximum packet _duplication:", "times", "Maximum number of extra requests to send beoynd the initial one if a server does not respond" )
			, ("Throughput _limit:", "ms", "Should be set as low as possible as long as pings from \"Refresh all servers\" remains the same as \"Refresh current\"") ]
	return (tbl, rt)


framed :: ContainerClass w => String -> w -> IO VBox
framed title box = do
	l <- labelNew (Just title)
	miscSetAlignment l 0 0
	labelSetAttributes l [AttrWeight 0 (-1) WeightBold]

	align <- alignmentNew 0.5 0.5 1 1
	alignmentSetPadding align 0 0 spacingBig 0
	containerAdd align box

	vb <- vBoxNew False spacing
	boxPackStart vb l PackNatural 0
	boxPackStart vb align PackNatural 0

	return vb

numberedColors :: IO (Table, [(ColorButton, CheckButton)])
numberedColors = do
	tbl <- paddedTableNew
	let easyAttach pos lbl  = do
		a <- labelNew (Just lbl)
		b <- colorButtonNew
		c <- checkButtonNew
		on c toggled $
			widgetSetSensitive b =<< toggleButtonGetActive c
		miscSetAlignment a 0.5 0
		tableAttach tbl a pos (pos+1) 0 1 [Fill] [] 0 0
		tableAttach tbl b pos (pos+1) 1 2 [Fill] [] 0 0
		tableAttach tbl c pos (pos+1) 2 3 [] [] 0 0
		return (b, c)

	xs <- zipWithM easyAttach [0..]  ["^0", "^1", "^2", "^3", "^4", "^5", "^6", "^7"]
	return (tbl, xs)


-- Gtk fails yet again and doesn't offer something like this by default
pathSelectionEntryNew :: Window -> IO (HBox, Entry)
pathSelectionEntryNew parent = do
	box	<- hBoxNew False 0
	button	<- buttonNew
	set button [ buttonImage :=> imageNewFromStock stockOpen (IconSizeUser 1) ]
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

paddedTableNew :: IO Table
paddedTableNew = do
	tbl <- tableNew 0 0 False
	set tbl	[ tableRowSpacing	:= spacingHalf
		, tableColumnSpacing	:= spacing ]
	return tbl

colorToHex :: Color -> String
colorToHex (Color a b c) = printf "#%02x%02x%02x" (f a) (f b) (f c)
	where f = (`quot` 0x100)

hexToColor :: String -> Color
hexToColor ('#':a:b:c:d:e:g:_)	= Color (f a b) (f c d) (f e g)
	where f x y = fromIntegral $ (digitToInt x * 0x10 + digitToInt y) * 0x100
hexToColor ('#':a:b:c:_)	= Color (f a) (f b) (f c)
	where f x = fromIntegral $ digitToInt x * 0x1000
hexToColor _			= Color 0 0 0
