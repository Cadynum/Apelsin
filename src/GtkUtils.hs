module GtkUtils where
import Graphics.UI.Gtk
import Control.Applicative
import Data.Maybe
import Monad2
import Data.ByteString (ByteString)

scrollIt, scrollItV :: WidgetClass widget => widget -> PolicyType -> PolicyType -> IO ScrolledWindow
scrollIt widget pol1 pol2 = do
	scroll <- scrolledWindowNew Nothing Nothing
	scrolledWindowSetPolicy scroll pol1 pol2
	containerAdd scroll widget
	return scroll

scrollItV widget pol1 pol2 = do
	scroll <- scrolledWindowNew Nothing Nothing
	scrolledWindowSetPolicy scroll pol1 pol2
	scrolledWindowAddWithViewport scroll widget
	vp <- binGetChild scroll
	whenJust vp $ \vp2 ->
		set (castToViewport vp2) [ viewportShadowType := ShadowNone ]
	set scroll [ scrolledWindowShadowType := ShadowNone ]
	return scroll


class GeneralTreeView c where
	getElementPath	:: TreeModelClass (a b) => c a b -> [Int] -> IO b
	getElementIter	:: TreeModelClass (a b) => c a b -> TreeIter -> IO b
	getStore	:: TreeModelClass (a b) => c a b -> a b
	getView		:: c a b -> TreeView


instance GeneralTreeView GenSimple where
	getView (GenSimple _ view)		= view
	getElementIter (GenSimple store _)	= treeModelGetRow store
	getElementPath (GenSimple store _) path	= treeModelGetRow store =<< getIterUnsafe store path
	getStore (GenSimple store _)		= store


instance GeneralTreeView GenFilterSort where
	getStore (GenFilterSort store _ _ _)	= store
	getView (GenFilterSort _ _ _ view)	= view
	getElementPath g@(GenFilterSort _ _ sorted _) path = getElementIter g =<< getIterUnsafe sorted path
	getElementIter (GenFilterSort store filtered sorted _) it =
		treeModelSortConvertIterToChildIter sorted it >>=
		treeModelFilterConvertIterToChildIter filtered >>=
		treeModelGetRow store

data GenCellRend i	= RendText2 ByteString (i -> [AttrOp CellRendererText])
			| RendMarkup ByteString (i -> [AttrOp CellRendererText])
			| RendPixbuf2 (i -> [AttrOp CellRendererPixbuf])

data GenSimple store a where
	GenSimple	:: (TypedTreeModelClass store, TreeModelClass (store a))
			=> !(store a)
			-> !TreeView
			-> GenSimple store a
data GenFilterSort store a where
	GenFilterSort	:: ( TreeModelClass (store a), TypedTreeModelClass store
			   , TreeModelClass filter, TreeModelFilterClass filter
			   , TreeModelClass sort, TreeModelSortClass sort, TreeSortableClass sort)
			=> !(store a)
			-> !filter
			-> !sort
			-> !TreeView
			-> GenFilterSort store a

newGenSimple :: (TypedTreeModelClass store, TreeModelClass (store a))
             => store a
             -> IO (GenSimple store a)
newGenSimple store = do
	view	<- treeViewNewWithModel store
	return (GenSimple store view)

newGenFilterSort :: (TypedTreeModelClass store, TreeModelClass (store a))
             => store a
             -> IO (GenFilterSort store a)
newGenFilterSort  store = do
	filtered	<- treeModelFilterNew store []
	sorted		<- treeModelSortNewWithModel filtered
	view		<- treeViewNewWithModel sorted
	treeSortableSetDefaultSortFunc sorted Nothing
	return (GenFilterSort store filtered sorted view)

addColumn :: GenSimple a e -> String -> Bool -> [AttrOp CellRendererText] -> (CellRendererText -> e -> IO ()) -> IO Int
addColumn gen@(GenSimple store view) title expand rendOpts f = do
	col <- treeViewColumnNew
	set col	[ treeViewColumnTitle := title
		, treeViewColumnExpand := expand ]
	rend <- cellRendererTextNew
	set rend rendOpts
	set rend [cellTextEllipsize := EllipsizeEnd]
	cellLayoutPackStart col rend True
	cellLayoutSetAttributeFunc col rend store $ \iter -> do
		item <- getElementIter gen iter
		f rend item
	treeViewAppendColumn view col

addColumnFS :: CellRendererClass rend
	=> GenFilterSort a e
	-> String
	-> Bool
	-> Maybe (e -> e -> Ordering)
	-> IO rend
	-> [AttrOp rend]
	-> (rend -> e -> IO ())
	-> IO ()
addColumnFS gen@(GenFilterSort store filtered sorted view) title expand sortf mkRend rendOpts f = do
	col <- treeViewColumnNew
	set col	[ treeViewColumnTitle := title
		, treeViewColumnExpand := expand ]
	rend <- mkRend
	set rend rendOpts
	cellLayoutPackStart col rend True
	cellLayoutSetAttributeFunc col rend sorted $ \iter -> do
		item <- getElementIter gen iter
		f rend item

	n <- pred <$> treeViewAppendColumn view col
	whenJust sortf $ \g -> do
		treeViewColumnSetSortColumnId col n
		treeSortableSetSortFunc sorted n $ \it1 it2 -> do
			rit1 <- treeModelFilterConvertIterToChildIter filtered it1
			rit2 <- treeModelFilterConvertIterToChildIter filtered it2
			g <$> treeModelGetRow store rit1 <*> treeModelGetRow store rit2



getIterUnsafe :: TreeModelClass self => self -> TreePath -> IO TreeIter
getIterUnsafe model path =
	fromMaybe (error "getElement: Imposssible error") <$> treeModelGetIter model path


gtkPopup :: MessageType -> String -> IO ()
gtkPopup what str = do
	a <- messageDialogNew Nothing [DialogDestroyWithParent, DialogModal]
		what ButtonsOk str
	set a [ windowWindowPosition := WinPosCenter]
	dialogRun a
	widgetDestroy a

gtkWarn, gtkError :: String -> IO ()
gtkWarn = gtkPopup MessageWarning
gtkError = gtkPopup MessageError

