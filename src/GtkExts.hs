module GtkExts (
	EntryIconPosition(..)
	, entrySetIconFromStock
	, frameNew
	, cellSetText
	, cellSetMarkup
) where
import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.GtkInternals
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Enums
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Data.ByteString.Char8

entrySetIconFromStock	:: EntryClass self => self
			-> EntryIconPosition
			-> StockId
			-> IO ()
entrySetIconFromStock entry iconPos stockId =
	withForeignPtr arg1 $ \argPtr1 ->
	withUTFString stockId $ \stockIdPtr ->
	gtk_entry_set_icon_from_stock argPtr1 arg2 stockIdPtr
	where
	Entry arg1	= toEntry entry
	arg2		= (fromIntegral . fromEnum) iconPos

foreign import ccall unsafe "gtk_entry_set_icon_from_stock"
	gtk_entry_set_icon_from_stock :: Ptr Entry -> CInt -> Ptr CChar -> IO ()



frameNew :: Maybe String -> IO Frame
frameNew label =
	maybeWith withUTFString label $ \labelPtr ->
	makeNewObject mkFrame (gtk_frame_new labelPtr)

foreign import ccall unsafe "gtk_frame_new"
	gtk_frame_new :: Ptr CChar -> IO (Ptr Frame)

cellSetText :: CellRendererTextClass obj => obj -> ByteString -> IO ()
cellSetText rend text = 
	withForeignPtr w $ \wPtr ->
	useAsCString text $ \textPtr ->
	g_object_set wPtr prop_text textPtr nullPtr
	where CellRenderer w = toCellRenderer rend

cellSetMarkup :: CellRendererTextClass obj => obj -> ByteString -> IO ()
cellSetMarkup rend text = 
	withForeignPtr w $ \wPtr ->
	useAsCString text $ \textPtr ->
	g_object_set wPtr prop_markup textPtr nullPtr
	where CellRenderer w = toCellRenderer rend

prop_text :: CString
prop_text = unsafePerformIO $ newCString "markup"

prop_markup :: CString
prop_markup = unsafePerformIO $ newCString "markup"


foreign import ccall unsafe "g_object_set"
	g_object_set :: Ptr CellRenderer -> CString -> CString -> Ptr () -> IO ()

