module About where
import Graphics.UI.Gtk
import Constants
import Data.Version
import Paths_apelsin (version)

newAbout :: WindowClass w => w -> IO ()
newAbout win = do
	about <- aboutDialogNew
	aboutDialogSetUrlHook openInBrowser

	set about
		[ windowWindowPosition	:= WinPosCenterOnParent
		, windowTransientFor	:= win
		, aboutDialogProgramName:= programName
		, aboutDialogVersion	:= showVersion version
		, aboutDialogCopyright	:= "Copyright © 2011-2013\nChristoffer Öjeling <christoffer@ojeling.net>"
		, aboutDialogComments	:= "A tremulous server and community browser\nLicense: GPLv3"
		, aboutDialogWebsite	:= "http://ojeling.net/apelsin"
		]
	dialogRun about
	widgetDestroy about
