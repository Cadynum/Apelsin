module FilterBar where

import Graphics.UI.Gtk
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Data.Char
import Network.Tremulous.ByteStringUtils as B

import Constants


newFilterBar :: (TreeModelClass self, TreeModelFilterClass self) => self -> Label -> String
	-> IO (HBox, IORef B.ByteString)
newFilterBar filtered stat initial  = do
	current <- newIORef ""
	
	-- Filterbar
	ent <- entryNew
	set ent [ widgetHasFocus	:= True
		, widgetIsFocus		:= True
		, widgetCanDefault	:= True
		, entryText		:= initial ]
	lbl <- labelNewWithMnemonic "_Filter:"
	set lbl [ labelMnemonicWidget := ent ]
	
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
		return True
	f 
	onKeyRelease ent (const f) 
		
	return (findbar, current)

smartFilter :: B.ByteString -> [B.ByteString] -> Bool
smartFilter raw xs = all (\x -> any (x `B.isInfixOf`) xs) search 
	where
	search	= B.splitfilter ' ' raw
