module GtkUtils where
import Graphics.UI.Gtk
import Control.Monad 

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
	Just vp <- binGetChild scroll
	set scroll [ scrolledWindowShadowType := ShadowNone ]
	set (castToViewport vp) [ viewportShadowType := ShadowNone ]
	return scroll
	

simpleListView :: [(String, Bool, a -> String)] -> IO (ListStore a, TreeView)
simpleListView headers = do
	model <- listStoreNew []
	view <- treeViewNewWithModel model
	select <- treeViewGetSelection view
	treeSelectionSetMode select SelectionNone
	addColumns model view $
		map (\(title, isName, f) -> (title, if isName then 0 else 1, isName, isName, isName, f)) headers
	return (model, view)
	
	

newLabeledFrame :: String -> IO Frame
newLabeledFrame lbl = do
	frame <- frameNew
	frameSetLabel frame lbl
	return frame
	
addColumns :: (TreeViewClass view, TreeModelClass (model row), TypedTreeModelClass model) =>
	model row -> view -> [(String, Float, Bool, Bool, Bool, row -> String)] -> IO ()
addColumns model view xs = mapM_ g xs where
	g (title, align, format, expand, ellipsize, f) = do
		col <- treeViewColumnNew
		set col	[ treeViewColumnTitle := title
			, treeViewColumnExpand := expand ]
		rend <- cellRendererTextNew
		set rend [ cellXAlign := align]
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
addColumnsFilter :: (TreeViewClass self1, TreeModelFilterClass self, TreeModelClass self, TypedTreeModelClass model) =>
	model t -> self -> self1 -> [(String, Bool, t -> String)] -> IO ()
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

data RendType i = RendText (i -> [AttrOp CellRendererText]) | RendPixbuf (i -> [AttrOp CellRendererPixbuf])
addColumnsFilterSort :: (TreeViewClass self, TreeSortableClass self1, TreeModelSortClass self1,
	TreeModelFilterClass self2, TreeModelClass self1, TypedTreeModelClass model) =>
	model t -> self2 -> self1 -> self -> Int -> SortType
	-> [(String, Bool, RendType t, Maybe (t -> t -> Ordering))]
	-> IO ()
addColumnsFilterSort raw filtered sorted view defaultSort sortType xs = zipWithM_ f [0..] xs where
	f n (title, expand, rt, sortf) = do
		col <- treeViewColumnNew
		set col	[ treeViewColumnTitle := title
			, treeViewColumnExpand := expand ]
		case rt of
			RendText attr -> do
				rend <- cellRendererTextNew
				cellLayoutPackStart col rend True
				cellLayoutSetAttributeFunc col rend sorted $ \iter -> do
					cIter	<- treeModelSortConvertIterToChildIter sorted iter
					rcIter	<- treeModelFilterConvertIterToChildIter filtered cIter
					item	<- treeModelGetRow raw rcIter
					set rend $ attr item
			RendPixbuf attr -> do
				rend <- cellRendererPixbufNew
				cellLayoutPackStart col rend True
				cellLayoutSetAttributeFunc col rend sorted $ \iter -> do
					cIter	<- treeModelSortConvertIterToChildIter sorted iter
					rcIter	<- treeModelFilterConvertIterToChildIter filtered cIter
					item	<- treeModelGetRow raw rcIter
					set rend $ attr item
		treeViewAppendColumn view col
		case sortf of
			Nothing	-> return ()
			Just g	-> do
				col `treeViewColumnSetSortColumnId` n
				treeSortableSetSortFunc sorted n $ \it1 it2 -> do
					rit1	<- treeModelFilterConvertIterToChildIter filtered it1
					rit2	<- treeModelFilterConvertIterToChildIter filtered it2
					xort raw g rit1 rit2
		treeSortableSetDefaultSortFunc sorted Nothing
		treeSortableSetSortColumnId sorted defaultSort sortType
		
getElementFS :: (TreeModelClass self, TreeModelFilterClass self1, TreeModelSortClass self, TypedTreeModelClass model) =>
	model b -> self -> self1 -> TreePath -> IO b
getElementFS store sorted filtered x = do
	Just vIter	<- treeModelGetIter sorted x
	sIter		<- treeModelSortConvertIterToChildIter sorted vIter
	fIter		<- treeModelFilterConvertIterToChildIter filtered sIter
	treeModelGetRow store fIter

getElementF :: (TreeModelClass self, TreeModelFilterClass self, TypedTreeModelClass model) =>
	model b -> self -> TreePath -> IO b
getElementF store filtered path = do
	Just vIter	<- treeModelGetIter filtered path
	iter		<- treeModelFilterConvertIterToChildIter filtered vIter
	treeModelGetRow store iter
	
getElement :: (TreeModelClass (model m), TypedTreeModelClass model) => model m -> TreePath -> IO m
getElement raw path = do
	Just iter	<- treeModelGetIter raw path
	treeModelGetRow raw iter
		
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

xort :: TypedTreeModelClass model => model t -> (t -> t -> b) -> TreeIter -> TreeIter -> IO b
xort model g it1 it2 = do
a <- treeModelGetRow model it1 
b <- treeModelGetRow model  it2
return $ g a b

