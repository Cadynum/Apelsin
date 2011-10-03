module GtkExts (
	EntryIconPosition(..)
	, entrySetIconFromStock
	, cellSetText
	, cellSetMarkup
) where
import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.GtkInternals
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Enums
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
	gtk_entry_set_icon_from_stock :: Ptr Entry -> CInt -> CString -> IO ()


cellSetText :: CellRendererText -> ByteString -> IO ()
cellSetText (CellRendererText obj) text =
	withForeignPtr obj $ \wPtr ->
	useAsCString text $ \textPtr ->
	g_object_set wPtr prop_text textPtr nullPtr

cellSetMarkup :: CellRendererText -> ByteString -> IO ()
cellSetMarkup (CellRendererText obj) text =
	withForeignPtr obj $ \wPtr ->
	useAsCString text $ \textPtr ->
	g_object_set wPtr prop_markup textPtr nullPtr

prop_text, prop_markup :: CString
prop_text = unsafePerformIO $ newCString "text"
prop_markup = unsafePerformIO $ newCString "markup"

foreign import ccall unsafe "g_object_set"
	g_object_set :: Ptr CellRendererText -> CString -> CString -> Ptr () -> IO ()
