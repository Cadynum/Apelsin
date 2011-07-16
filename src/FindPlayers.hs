module FindPlayers where
import Graphics.UI.Gtk

import Data.IORef
import Data.List
import Data.Ord
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

newFindPlayers :: Bundle -> SetCurrent -> IO (VBox, PolledHook, Entry)
newFindPlayers Bundle{..} setServer = do
	Config {..}	<- atomically $ readTMVar mconfig
	raw		<- listStoreNew []
	filtered	<- treeModelFilterNew raw []
	view		<- treeViewNewWithModel filtered
	
	addColumnsFilter raw filtered view [
		  ("Name", True, pangoPretty colors . fst)
		, ("Server", True, pangoPretty colors . hostname . snd) 
		]

	(infobox, statNow, statTot)	<- newInfobox "players"
	(filterbar, current, ent)	<- newFilterBar filtered statNow filterPlayers
	
	treeModelFilterSetVisibleFunc filtered $ \iter -> do
		(item, GameServer{..}) <- treeModelGetRow raw iter
		s <- readIORef current
		return $ B.null s || smartFilter s [
				  cleanedCase item
				, proto2string protocol
				, maybe "" cleanedCase gamemod
				]
		
	let updateFilter PollResult{..} = do
		listStoreClear raw
		let plist = sortBy (comparing fst) (makePlayerNameList polled)
		mapM_ (listStoreAppend raw) plist
		treeModelFilterRefilter filtered
		set statTot [ labelText := show (length plist) ]
		n <- treeModelIterNChildren filtered Nothing
		set statNow [ labelText := show n ]
						
	on view cursorChanged $ do
		(path, _) <- treeViewGetCursor view
		setServer False . snd =<< getElementF raw filtered path

	on view rowActivated $ \path _ -> do
		setServer True . snd =<< getElementF raw filtered path
	
	scroll <- scrollIt view PolicyAutomatic PolicyAlways
	
	box <- vBoxNew False 0
	boxPackStart box filterbar PackNatural spacing
	boxPackStart box scroll PackGrow 0
	boxPackStart box infobox PackNatural 0
	
	return (box, updateFilter, ent)
