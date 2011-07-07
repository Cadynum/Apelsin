module About where
import Graphics.UI.Gtk
import Constants

newAbout :: WindowClass w => w -> IO ()
newAbout win = do
	about <- aboutDialogNew
	set about [
		  windowWindowPosition := WinPosCenterOnParent
		, windowTransientFor	:= win
		, aboutDialogProgramName:= programName
		, aboutDialogVersion	:= "1.0"
		, aboutDialogCopyright	:= "Copyright © 2011\nChristoffer Öjeling <christoffer@ojeling.net>"
		, aboutDialogComments	:= "A tremulous server and community browser\nLicense: GPLv3"
		, aboutDialogWebsite	:= "http://ojeling.net"
		]
	dialogRun about
	widgetDestroy about
