module GtkUtils where
import Graphics.UI.Gtk
import Control.Monad (when)

whenJust :: Monad m => (Maybe a) -> (a -> m ()) -> m ()
whenJust x f = case x of
	Nothing	-> return ()
	Just a	-> f a 
	
			
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
	--set scroll [ scrolledWindowShadowType := ShadowNone ]
	Just vp <- binGetChild scroll
	set (castToViewport vp) [ viewportShadowType  := ShadowNone ]
	return scroll
	

simpleListView :: [(String, Bool, (a -> String))] -> IO (ListStore a, TreeView)
simpleListView headers = do
	model <- listStoreNew []
	view <- treeViewNewWithModel model
	select <- treeViewGetSelection view
	treeSelectionSetMode select SelectionNone
	addColumns model view $
		map (\(title, isName, f) -> (title, isName, isName, isName, f)) headers
	return (model, view)
	
	

newLabeledFrame :: String -> IO Frame
newLabeledFrame lbl = do
	frame <- frameNew
	frameSetLabel frame lbl
	return frame
	
	
notebookAppendMnemonic :: (NotebookClass self, WidgetClass child) => self -> child -> String -> IO Int
notebookAppendMnemonic nb child txt = do
	n <- notebookAppendPage nb child ""
	lbl <- labelNewWithMnemonic txt
	notebookSetTabLabel nb child lbl
	return n

addColumns :: (TreeViewClass view, TreeModelClass (model row), TypedTreeModelClass model) =>
	model row -> view -> [(String, Bool, Bool, Bool, row -> String)] -> IO ()
addColumns model view xs = mapM_ g xs where
	g (title, format, expand, ellipsize, f) = do
		col <- treeViewColumnNew
		set col	[ treeViewColumnTitle := title
			, treeViewColumnExpand := expand ]
		rend <- cellRendererTextNew
		when ellipsize $
			set rend [ cellTextEllipsizeSet := True, cellTextEllipsize := EllipsizeEnd]
		cellLayoutPackStart col rend True
		cellLayoutSetAttributes col rend model $ \row -> if format
			then [ cellTextMarkup := Just (f row) ]
			else [ cellText := f row ]
		treeViewAppendColumn view col
		
{-
addColumnsSort raw model view xs = sequence_ $ zipWith f (iterate (+1) 0) xs where
	f n (title, format, expand, showf,  sortf) = do
		col <- treeViewColumnNew
		set col	[ treeViewColumnTitle := title
			, treeViewColumnExpand := expand ]
		rend <- cellRendererTextNew
		cellLayoutPackStart col rend True
		cellLayoutSetAttributeFunc col rend model $ \iter -> do
			cIter	<- treeModelSortConvertIterToChildIter model iter
			item	<- treeModelGetRow raw cIter
			set rend $ if format 
				then [ cellTextMarkup := Just (showf item) ]
				else [ cellText := showf item ]
		treeViewAppendColumn view col
		case sortf of
			Nothing	-> return ()
			Just g	-> do
				col `treeViewColumnSetSortColumnId` n
				treeSortableSetSortFunc model n (xort raw g)
-}			
addColumnsFilter raw model view xs = mapM_ f xs where
	f (title, expand, showf) = do
		col <- treeViewColumnNew
		set col	[ treeViewColumnTitle := title
			, treeViewColumnExpand := expand ]
		rend <- cellRendererTextNew
		when expand $
			set rend [ cellTextEllipsize := EllipsizeEnd]
		cellLayoutPackStart col rend True
		cellLayoutSetAttributeFunc col rend model $ \iter -> do
			cIter	<- treeModelFilterConvertIterToChildIter model iter
			item	<- treeModelGetRow raw cIter
			set rend [ cellTextMarkup := Just (showf item) ]
		treeViewAppendColumn view col
		
		
addColumnsFilterSort raw filtered sorted view defaultSort xs = sequence_ $ zipWith f (iterate (+1) 0) xs where
	f n (title, format, expand, ellipsize, showf,  sortf) = do
		col <- treeViewColumnNew
		set col	[ treeViewColumnTitle := title
			, treeViewColumnExpand := expand ]
		rend <- cellRendererTextNew
		when ellipsize $
			set rend [ cellTextEllipsizeSet := True, cellTextEllipsize := EllipsizeEnd]
		cellLayoutPackStart col rend True
		cellLayoutSetAttributeFunc col rend sorted $ \iter -> do
			cIter	<- treeModelSortConvertIterToChildIter sorted iter
			rcIter	<- treeModelFilterConvertIterToChildIter filtered cIter
			item	<- treeModelGetRow raw rcIter
			set rend $ if format 
				then [ cellTextMarkup := Just (showf item) ]
				else [ cellText := showf item ] 
		treeViewAppendColumn view col
		case sortf of
			Nothing	-> return ()
			Just g	-> do
				col `treeViewColumnSetSortColumnId` n
				treeSortableSetSortFunc sorted n $ \it1 it2 -> do
					rit1	<- treeModelFilterConvertIterToChildIter filtered it1
					rit2	<- treeModelFilterConvertIterToChildIter filtered it2
					xort raw g rit1 rit2
		whenJust defaultSort $ \a -> do
			treeSortableSetDefaultSortFunc sorted $ Just $ \it1 it2 -> do
				rit1	<- treeModelFilterConvertIterToChildIter filtered it1
				rit2	<- treeModelFilterConvertIterToChildIter filtered it2
				xort raw a rit1 rit2
				
gtkPopup :: MessageType -> String -> IO ()
gtkPopup what str = do
	a <- messageDialogNew Nothing [DialogDestroyWithParent, DialogModal]
		what ButtonsOk str
	dialogRun a
	widgetDestroy a

gtkWarn, gtkError :: String -> IO ()
gtkWarn = gtkPopup MessageWarning
gtkError = gtkPopup MessageError

xort :: TypedTreeModelClass model => model t -> (t -> t -> b) -> TreeIter -> TreeIter -> IO b
xort model g it1 it2 = do
a <- treeModelGetRow model it1 
b <- treeModelGetRow model  it2
return $ g a b

