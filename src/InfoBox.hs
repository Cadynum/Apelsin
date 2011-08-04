module InfoBox (newInfobox, newInfoboxBrowser) where
import Graphics.UI.Gtk


newInfobox :: String -> IO (VBox, Label, Label)
newInfobox what = do
	lst <- mapM (labelNew . Just)
		["Showing", "0", " out of", "0", ' ' : what]
		
	let (_:a:_:b:_) = lst
	mapM_ (\x -> set x [ labelWidthChars := 4 ] >> miscSetAlignment x 1 0) [a,b]
	
	bar <- mkStatusBar lst
	return (bar, a, b)

newInfoboxBrowser :: IO (VBox, Label, Label, Label)
newInfoboxBrowser = do
	lst <- mapM (labelNew . Just)
		["Showing", "0", " out of", "0", " servers, with", "0", " not responding" ]

	let (_:a:_:b:_:c:_) = lst
	mapM_ (\x -> set x [ labelWidthChars := 4 ] >> miscSetAlignment x 1 0) [a,b,c]
	set c [ labelWidthChars := 3 ]
	
	bar <- mkStatusBar lst
	return (bar, a, b, c)

mkStatusBar :: WidgetClass child => [child] -> IO VBox
mkStatusBar xs = do
	box <- hBoxNew False 0
	mapM_ (\x -> boxPackStart box x PackNatural 0) xs
	vb <- vBoxNew False 0
	sep <- hSeparatorNew
	boxPackStart vb sep PackNatural 0
	boxPackStart vb box PackNatural 0
	return vb
	
	
