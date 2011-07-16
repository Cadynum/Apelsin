module GtkExts (
	EntryIconPosition(..)
	, entrySetIconFromStock
) where
import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.GtkInternals
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Enums

entrySetIconFromStock	:: EntryClass self => self
			-> EntryIconPosition
			-> StockId
			-> IO ()
entrySetIconFromStock entry iconPos stockId =
	(\(Entry arg1) arg2 ->
		withForeignPtr arg1 $ \argPtr1 ->
		withUTFString stockId $ \stockIdPtr ->
		gtk_entry_set_icon_from_stock argPtr1 arg2 stockIdPtr)
			(toEntry entry)
			((fromIntegral . fromEnum) iconPos)

foreign import ccall safe "gtk_entry_set_icon_from_stock"
	gtk_entry_set_icon_from_stock :: Ptr Entry -> CInt -> Ptr CChar -> IO ()
