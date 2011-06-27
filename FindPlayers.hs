module FindPlayers where
import Graphics.UI.Gtk

import Data.IORef
import qualified Data.ByteString.Char8 as B
import Tremulous.Protocol
import Tremulous.Util

import STM2
import GtkUtils
import TremFormatting
import FilterBar
import InfoBox
import Constants
import Config

newFindPlayers :: TMVar [GameServer] -> TMVar Config -> (GameServer -> IO ()) -> IO (VBox, IO ())
newFindPlayers mvar mconfig currentSet = do
	Config {colors} <- atomically $ readTMVar mconfig
	rawmodel	<- listStoreNew []
	model		<- treeModelFilterNew rawmodel []
	view		<- treeViewNewWithModel model
	
	addColumnsFilter rawmodel model view [
		  ("Name", True, pangoPretty colors . fst)
		, ("Server", True, unfuckName colors . hostname . snd) 
		]

	(infobox, statNow, statTot) <- newInfobox "players"
	Config {filterPlayers} <- atomically $ readTMVar mconfig
	(filterbar, current)	<- newFilterBar model statNow filterPlayers
	
	treeModelFilterSetVisibleFunc model $ \iter -> do
		(item,_) <- treeModelGetRow rawmodel iter
		s <- readIORef current
		return $ if B.null s
			then True
			else s `B.isInfixOf` (cleanedCase item)
		
	
	
	
	let updateFilter = do
		tst <- atomically $ tryReadTMVar mvar
		case tst of 
			Nothing -> return ()
			Just a	-> do
				listStoreClear rawmodel
				let plist = makePlayerList a
				mapM_ (listStoreAppend rawmodel) plist
				treeModelFilterRefilter model
				set statTot [ labelText := show (length plist) ]
				n <- treeModelIterNChildren model Nothing
				set statNow [ labelText := show n ]
						
	onCursorChanged view $ do
		(x, _) <- treeViewGetCursor view
		Just vIter <- treeModelGetIter model x
		iter	<-treeModelFilterConvertIterToChildIter model vIter
		gameserver <- treeModelGetRow rawmodel iter
		currentSet (snd gameserver)
		return ()
	
	scroll <- scrollIt view PolicyAutomatic PolicyAlways
	
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural g_SPACING
	boxPackStart box scroll PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateFilter)
