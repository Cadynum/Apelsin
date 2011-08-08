module FilterBar (newFilterBar, smartFilter) where

import Graphics.UI.Gtk
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char hiding (Control)
import Control.Monad
import Control.Monad.IO.Class
import Network.Tremulous.ByteStringUtils as B

import GtkExts
import Constants


newFilterBar :: (TreeModelClass self, TreeModelFilterClass self) => Window -> self -> Label -> String
	-> IO (HBox, IORef [Expr])
newFilterBar win filtered stat initial  = do
	current <- newIORef []

	-- Filterbar
	ent <- entryNew
	set ent [ entryText := initial ]
	entrySetIconFromStock ent EntryIconSecondary stockClear

	lbl <- labelNew (Just "Filter:")
	set lbl [ widgetTooltipText := Just "Ctrl+L or Ctrl+F" ]

	findbar <- hBoxNew False 0
	boxPackStart findbar lbl PackNatural spacingHalf
	boxPackStart findbar ent PackGrow spacingHalf

	let f = do
		str <- entryGetText ent
		writeIORef current (stringToExpr str)
		treeModelFilterRefilter filtered
		labelSetText stat . show =<< treeModelIterNChildren filtered Nothing
	f
	on ent editableChanged f

	on ent entryIconPress $
		const $ liftIO $ editableDeleteText ent 0 (-1)

	on win keyPressEvent $ do
		s	<- eventModifier
		k	<- map toLower `fmap` eventKeyName
		b 	<- liftIO $ widgetGetMapped ent
		let p	= s == [Control] && b && (k == "f" || k == "l")
		when p (liftIO (widgetGrabFocus ent))
		return p

	-- Focus on start
	on ent showSignal $ widgetGrabFocus ent

	return (findbar, current)


data Expr = Is !ByteString | Not !ByteString

mkExpr :: ByteString -> Expr
mkExpr ss = case B.uncons ss of
	Just ('-', xs)	| B.length xs > 0	-> Not (B.tail ss)
			| otherwise		-> Is xs
	_					-> Is ss

matchExpr :: Expr -> [ByteString] -> Bool
matchExpr (Is xs)  = any (xs `B.isInfixOf`)
matchExpr (Not xs) = all (not . (xs `B.isInfixOf`))

stringToExpr :: String -> [Expr]
stringToExpr = map mkExpr . B.splitfilter ' ' . B.pack . map toLower

smartFilter :: [Expr]-> [ByteString] -> Bool
smartFilter [] 	_	= True
smartFilter expr xs	= all (`matchExpr` xs) expr


