module FindPlayers where
import Graphics.UI.Gtk

import Data.IORef
import qualified Data.ByteString.Char8 as B
import Network.Tremulous.Protocol
import Network.Tremulous.Util

import Types
import GtkUtils
import TremFormatting
import FilterBar
import InfoBox
import Constants
import Config

newFindPlayers :: Bundle -> (GameServer -> IO ()) -> IO (VBox, IO ())
newFindPlayers Bundle{..} currentSet = do
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
		return $ B.null s || s `B.isInfixOf` (cleanedCase item)
		
	let updateFilter = do
		polled <- atomically $ readTMVar mpolled
		listStoreClear rawmodel
		let plist = makePlayerList polled
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
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scroll PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateFilter)
