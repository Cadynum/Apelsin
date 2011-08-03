module SettingsDialog where
import Graphics.UI.Gtk
import Control.Applicative
import Network.Tremulous.Protocol
import Control.Monad

import IndividualServerSettings
import TremFormatting
import Constants

newSettingsDialog :: Window -> ColorArray -> Bool -> GameServer -> ServerArg -> IO (Maybe ServerArg)
newSettingsDialog win colors requirepw GameServer{..} ServerArg{..} = do
	dia <- dialogNew
	set dia	[ windowTitle			:= "Server settings"
		, windowWindowPosition		:= WinPosCenterOnParent
		, windowTransientFor		:= win
		]

	dialogAddButton dia stockCancel ResponseCancel
	dialogAddButton dia stockOk     ResponseOk
	dialogSetDefaultResponse dia ResponseOk

	srv <- labelNew $ Just $ pangoPretty colors hostname
	set srv [ labelWrap		:= True
		, labelJustify		:= JustifyCenter
		, labelUseMarkup	:= True
		, labelAttributes 	:= [AttrWeight 0 (-1) WeightBold, AttrScale 0 (-1) 1.2]
		]
	labelSetLineWrapMode srv WrapPartialWords

	ip <- labelNew $ Just $ show address
	
	tbl <- tableNew 0 0 False
	set tbl	[ tableRowSpacing	:= spacingHalf
		, tableColumnSpacing	:= spacing
		]
		
	let easyAttach pos (lbl, tip)  = do
		a <- labelNewWithMnemonic lbl
		b <- entryNew
		entrySetActivatesDefault b True
		when (pos == 1 && requirepw) $ do
			labelSetAttributes a [AttrWeight 0 (-1) WeightBold]
			
		set a	[ labelMnemonicWidget	:= b
			, widgetTooltipText	:= Just tip ]
		miscSetAlignment a 0 0.5
		tableAttach tbl a 0 1 pos (pos+1) [Fill] [] 0 0
		tableAttach tbl b 1 2 pos (pos+1) [Expand, Fill] [] 0 0
		return b

	[name, pass, rcon] <- zipWithM easyAttach [0..]
		[ ("Override _name:", "Set a custom name that will only be used on this server")
		, ("_Password:", "Server password")
		, ("_Rcon:", "Rcon password")
		]
	set pass [ entryText := serverPass ]
	set rcon [ entryText := serverRcon ]
	set name [ entryText := serverName ]

	 
	
	box <- vBoxNew False spacing
	set box [ containerBorderWidth := spacing ]
	boxPackStart box srv PackNatural 0
	boxPackStart box ip PackNatural 0
	when requirepw $ do
		l <- labelNew (Just "This server requires a password!")
		labelSetAttributes l [AttrWeight 0 (-1) WeightBold]
		boxPackStart box l PackNatural 0
	boxPackStart box tbl PackNatural 0
	
	dbox <- dialogGetUpper dia
	boxPackStart dbox box PackNatural 0
	widgetShowAll dbox
	when requirepw (widgetGrabFocus pass)
	
	answer <- dialogRun dia
	case answer of
		ResponseOk 	-> Just <$> (ServerArg
						<$> get pass entryText
						<*> get rcon entryText
						<*> get name entryText)
					<* widgetDestroy dia
		_		-> widgetDestroy dia >> return Nothing
	