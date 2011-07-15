module FilterBar (newFilterBar, smartFilter) where

import Graphics.UI.Gtk
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Network.Tremulous.ByteStringUtils as B

import Constants


newFilterBar :: (TreeModelClass self, TreeModelFilterClass self) => self -> Label -> String
	-> IO (HBox, IORef ByteString, Entry)
newFilterBar filtered stat initial  = do
	current <- newIORef ""
	
	-- Filterbar
	ent <- entryNew
	set ent [ entryText := initial ]
		
	lbl <- labelNew (Just "Filter:")
	set lbl [ widgetTooltipText := Just "Ctrl+L or Ctrl+F" ]
	
	findbar <- hBoxNew False spacing
	boxPackStart findbar lbl PackNatural 0
	boxPackStart findbar ent PackGrow 0
	
	let f = do
		rawstr <- entryGetText ent
		let str = B.pack $ map toLower rawstr
		writeIORef current str
		treeModelFilterRefilter filtered
		n <- treeModelIterNChildren filtered Nothing
		set stat [ labelText := show n ]
	f 
	on ent editableChanged f
		
	return (findbar, current, ent)

	
data Expr = Is !ByteString | Not !ByteString

mkExpr :: ByteString -> Expr
mkExpr ss = case (B.head ss, B.tail ss) of
	('-', xs) | B.length xs > 0	-> Not (B.tail ss)
                  | otherwise		-> Is xs                 
	_				-> Is ss

matchExpr :: Expr -> [ByteString] -> Bool
matchExpr (Is xs)  = any (xs `B.isInfixOf`)
matchExpr (Not xs) = all (not . (xs `B.isInfixOf`))

smartFilter :: ByteString -> [ByteString] -> Bool
smartFilter raw xs = all (`matchExpr` xs) search 
	where
	search	= map mkExpr (B.splitfilter ' ' raw)
	
