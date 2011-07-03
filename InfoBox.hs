module InfoBox where
import Graphics.UI.Gtk


newInfobox :: String -> IO (VBox, Label, Label)
newInfobox what= do
	box <- hBoxNew False 0

	lst <- mapM (labelNew . Just)
		["Showing", "0", " out of", "0", " " ++ what]

	let (_:a:_:b:_) = lst
	mapM (\x -> set x [ labelWidthChars := 4 ] >> miscSetAlignment x 1 0) [a,b]
	

	mapM_ (\x -> boxPackStart box x PackNatural 0) lst

	vb <- vBoxNew False 0
	sep <- hSeparatorNew
	boxPackStart vb sep PackNatural 0
	boxPackStart vb box PackNatural 0

	return (vb, a, b)

newInfoboxBrowser :: IO (VBox, Label, Label, Label)
newInfoboxBrowser = do
	box <- hBoxNew False 0

	lst <- mapM (labelNew . Just)
		["Showing", "0", " out of", "0", " servers, with", "0", " not responding" ]

	let (_:a:_:b:_:c:_) = lst
	mapM (\x -> set x [ labelWidthChars := 4 ] >> miscSetAlignment x 1 0) [a,b,c]
	

	mapM_ (\x -> boxPackStart box x PackNatural 0) lst

	vb <- vBoxNew False 0
	sep <- hSeparatorNew
	boxPackStart vb sep PackNatural 0
	boxPackStart vb box PackNatural 0

	return (vb, a, b, c)
