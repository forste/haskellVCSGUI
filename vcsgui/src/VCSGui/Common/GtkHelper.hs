-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.GtkHelper
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | This module contains functions to help building a GTK GUI using GTKBuilder.
--
-----------------------------------------------------------------------------

module VCSGui.Common.GtkHelper (
-- * Typesynonyms to wrap Gtk objects (*Item)
--
-- | These are built after the following scheme:
--
-- @((Name of the item as in the gladefile), (actual gtk object), (getter, setter))@
--
-- Note however that you can (and should) use the functions 'getName', 'getItem', 'get' and 'set'.

    openGladeFile
    , getWindowFromGlade
    , getActionFromGlade
    , getLabelFromGlade
    , getTextEntryFromGlade
    , getTextViewFromGlade
    , getComboBoxFromGlade
    , getCheckButtonFromGlade
    , getButtonFromGlade
    , getTreeViewFromGlade
    , getTreeViewFromGladeCustomStore
    , addColumnToTreeView
    , addColumnToTreeView'
    , addTextColumnToTreeView
    , addTextColumnToTreeView'


    , getName
    , getItem
    , get
    , set

    , closeWin
    , registerClose
    , registerCloseAction
    , registerQuit
    , registerQuitAction

    , WindowItem
    , ActionItem
    , LabelItem
    , TextEntryItem
    , TextViewItem
    , ComboBoxItem
    , CheckButtonItem
    , TreeViewItem
    , ButtonItem
) where

import qualified Graphics.UI.Gtk as Gtk

import System.Directory
import Control.Monad.Trans(liftIO)
import System.IO (hPutStrLn, stderr)
import VCSGui.Common.Helpers (emptyListToNothing)

-- Typesynonyms
type WindowItem = (String, Gtk.Window, ())
type ActionItem = (String, Gtk.Action, ())
type LabelItem = (String, Gtk.Label, (IO (Maybe String), String -> IO ()))
type TextEntryItem = (String, Gtk.Entry, (IO (Maybe String), String -> IO ()))
type ComboBoxItem = (String, Gtk.ComboBox, (IO (Maybe String), [String] -> IO ()))
type TextViewItem = (String, Gtk.TextView, (IO (Maybe String), String -> IO ()))
type TreeViewItem a = (String, (Gtk.ListStore a, Gtk.TreeView), (IO (Maybe [a]), [a] -> IO ()))
type CheckButtonItem = (String, Gtk.CheckButton, (IO Bool, Bool -> IO()))
type ButtonItem = (String, Gtk.Button, (IO String, String -> IO()))

-- Type accessors

-- | return the name of this item (as in the gladefile)
getName :: (String, a, b) -> String
getName (n, _, _) = n

-- | return the Gtk object wrapped by given item
getItem :: (String, a, b) -> a
getItem (_, item, _) = item

-- | call teh get method of an *Item
get :: (String, a, (b, c)) -> b
get (_, _, (getter, _)) = getter

-- | call the set method of an *Item
set :: (String, a, (b, c)) -> c
set (_,_, (_, setter)) = setter


----------------------
-- *FromGlade
----------------------

-- | Open a gladefile with a new 'Gtk.Builder'.
openGladeFile :: FilePath -- ^ Gladefile to open.
    -> IO Gtk.Builder
openGladeFile fn = do
    builder <- Gtk.builderNew
    Gtk.builderAddFromFile builder fn
    return builder

-- | Get a 'WindowItem' from a gladefile.
getWindowFromGlade :: Gtk.Builder
    -> String -- ^ name of the window to get as specified in the gladefile.
    -> IO WindowItem
getWindowFromGlade builder name = do
    (a, b) <- wrapWidget builder Gtk.castToWindow name
    return (a, b, ())

-- | Get an 'ActionItem' from a gladefile.
getActionFromGlade :: Gtk.Builder
    -> String -- ^ name of the action to get as specified in the gladefile.
    -> IO ActionItem
getActionFromGlade builder name = do
    (a, b) <- wrapWidget builder Gtk.castToAction name
    return (a, b, ())

-- | Get an 'LabelItem' from a gladefile.
getLabelFromGlade :: Gtk.Builder
    -> String -- ^ name of the label to get as specified in the gladefile.
    -> IO LabelItem
getLabelFromGlade builder name = do
    (_, entry) <- wrapWidget builder Gtk.castToLabel name
    let getter = error "don't call get on a gtk label!" :: IO (Maybe String)
        setter val = Gtk.labelSetText entry val :: IO ()
    return (name, entry, (getter, setter))

-- | Get a 'ButtonItem' from a gladefile.
getButtonFromGlade :: Gtk.Builder
    -> String -- ^ name of the button to get as specified in the gladefile.
    -> IO ButtonItem
getButtonFromGlade builder name = do
    (_,btn) <- wrapWidget builder Gtk.castToButton name
    let getter = Gtk.buttonGetLabel btn :: IO String
        setter val = Gtk.buttonSetLabel btn val
    return (name, btn, (getter,setter))

-- | Get a 'TextEntryItem' from a gladefile.
getTextEntryFromGlade :: Gtk.Builder
    -> String -- ^ name of the text entry to get as specified in the gladefile.
    -> IO TextEntryItem
getTextEntryFromGlade builder name = do
    (_, entry) <- wrapWidget builder Gtk.castToEntry name
    let getter = fmap emptyListToNothing $ Gtk.entryGetText entry :: IO (Maybe String)
        setter val = Gtk.entrySetText entry val :: IO ()
    return (name, entry, (getter, setter))

-- | Get a 'ComboBoxItem' from a gladefile.
getComboBoxFromGlade :: Gtk.Builder
                    -> String -- ^ name of the combo box to get as specified in the gladefile.
                    -> IO ComboBoxItem
getComboBoxFromGlade builder name = do
    (_, combo) <- wrapWidget builder Gtk.castToComboBox name
    Gtk.comboBoxSetModelText combo
    let getter = Gtk.comboBoxGetActiveText combo  :: IO (Maybe String) -- get selected text
        setter entries = do -- fill with new entries
            store <- Gtk.comboBoxGetModelText combo
            Gtk.listStoreClear store
            mapM_ (Gtk.listStoreAppend store) entries
            return ()
    return (name, combo, (getter, setter))

-- | Get a 'TextViewItem' from a gladefile.
getTextViewFromGlade :: Gtk.Builder
    -> String -- ^ name of the text view to get as specified in the gladefile.
    -> IO TextViewItem
getTextViewFromGlade builder name =  do
        (_, entry)  <- wrapWidget builder Gtk.castToTextView name
        buffer <- Gtk.textViewGetBuffer entry
        let getter = getLongText buffer :: IO (Maybe String)
            setter = (\text -> Gtk.textBufferSetText buffer text) :: String -> IO ()
        return (name, entry, (getter, setter))
    where
    getLongText buffer = do
        start <- Gtk.textBufferGetStartIter buffer
        end <- Gtk.textBufferGetEndIter buffer
        isEmpty <- (Gtk.textIterEqual start end)
        if isEmpty then return Nothing else do
            s <- Gtk.textBufferGetText buffer start end True -- True to inclue hidden char
            return $ Just s

-- | Get a 'CheckButtonItem' from a gladefile.
getCheckButtonFromGlade :: Gtk.Builder
    -> String -- ^ name of the check button to get as specified in the gladefile.
    -> IO CheckButtonItem
getCheckButtonFromGlade builder name = do
        (_,bt) <- wrapWidget builder Gtk.castToCheckButton name
        let getter = Gtk.toggleButtonGetActive bt
            setter = (\bool -> Gtk.toggleButtonSetActive bt bool) :: Bool -> IO()
        return (name,bt, (getter,setter))

---------------------------------
-- TreeView
---------------------------------

-- | Get a 'TreeViewItem' from a gladefile.
getTreeViewFromGlade :: Gtk.Builder
    -> String -- ^ name of the tree view to get as specified in the gladefile.
    -> [a] -- ^ Content of the new tree view.
    -> IO (TreeViewItem a)
getTreeViewFromGlade builder name rows = do
    (_, tView) <- wrapWidget builder Gtk.castToTreeView name
    entry@(store, treeView) <- createStoreForTreeView tView rows
    let getter = getFromListStore entry
        setter = setToListStore entry
    return (name, (store, treeView), (getter, setter))

-- | Get a 'TreeViewItem' from a gladefile.
getTreeViewFromGladeCustomStore :: Gtk.Builder
                        -> String -- ^ name of the tree view to get as specified in the gladefile.
                        -> (Gtk.TreeView -> IO (Gtk.ListStore a)) -- ^ fn defining how to setup the liststore
                        -> IO (TreeViewItem a)
getTreeViewFromGladeCustomStore builder name setupListStore = do
    (_, tView) <- wrapWidget builder Gtk.castToTreeView name
    store <- setupListStore tView
    Gtk.treeViewSetModel tView store
    let getter = getFromListStore (store, tView)
        setter = setToListStore (store, tView)
    return (name, (store, tView), (getter, setter))

-- | Create a new 'Gtk.ListStore' for a 'Gtk.TreeView'.
createStoreForTreeView :: Gtk.TreeView -- ^ The created list store will be set the model for this TreeView.
    -> [a] -- ^ Content of the new store.
    -> IO (Gtk.ListStore a, Gtk.TreeView)
createStoreForTreeView listView rows = do
    listStore <- Gtk.listStoreNew rows
    Gtk.treeViewSetModel listView listStore
    return (listStore, listView)

-- | Get the content of a ListStore.
getFromListStore :: (Gtk.ListStore a, Gtk.TreeView)
    -> IO (Maybe [a]) -- ^ Nothing if the ListStore is empty.
getFromListStore (store, _) = do
    list <- Gtk.listStoreToList store
    if null list
        then return Nothing
        else return $ Just list

-- | Set the content of a ListStore.
setToListStore :: (Gtk.ListStore a, Gtk.TreeView)
    -> [a] -- ^ New content of the ListStore.
    -> IO ()
setToListStore (store, view) newList = do
    Gtk.listStoreClear store
    mapM_ (Gtk.listStoreAppend store) newList
    return ()

--
-- Various helpers
--

-- | Close a window.
closeWin :: WindowItem -> IO ()
closeWin win = (Gtk.widgetHide (getItem win))

-- | Close a window if 'Gtk.deleteEvent' occurs on this 'WindowItem'.
registerClose :: WindowItem -> IO ()
registerClose win = Gtk.on (getItem win) Gtk.deleteEvent (liftIO (closeWin win) >> return False) >> return ()

-- | Close a window if the specified action occurs on this 'WindowItem'.
registerCloseAction :: ActionItem -> WindowItem -> IO ()
registerCloseAction act win = Gtk.on (getItem act) Gtk.actionActivated (liftIO (closeWin win)) >> return ()

-- | Call 'Gtk.mainQuit' if 'Gtk.deleteEvent' occurs on this 'WindowItem'.
registerQuit :: WindowItem -> IO ()
registerQuit win = Gtk.on (getItem win) Gtk.deleteEvent (liftIO $ Gtk.mainQuit >> return False) >> return ()

-- | Call 'Gtk.mainQuit' if the specified action occurs on this 'WindowItem'.
registerQuitAction :: ActionItem -> IO ()
registerQuitAction act = Gtk.on (getItem act) Gtk.actionActivated (liftIO (Gtk.mainQuit)) >> return ()

-- TODO fun argument is not used. what was the purpos of this function?
-- | same as 'registerQuitAction' since second argument is ignored (?)
registerQuitWithCustomFun :: WindowItem
                -> IO () -- ^ custom fun
                -> IO ()
registerQuitWithCustomFun win fun = Gtk.on (getItem win) Gtk.deleteEvent (liftIO $ Gtk.mainQuit >> return False) >> return ()

-- | Add a column to given ListStore and TreeView using a mapping.
-- The mapping consists of a CellRenderer, the title and a function, that maps each row to attributes of the column
addColumnToTreeView :: Gtk.CellRendererClass r =>
    TreeViewItem a
    -> r -- ^ CellRenderer
    -> String -- ^ title
    -> (a -> [Gtk.AttrOp r]) -- ^ mapping
    -> IO ()
addColumnToTreeView (_, item, _) = do
    addColumnToTreeView' item
--    newCol <- Gtk.treeViewColumnNew
--    Gtk.set newCol [Gtk.treeViewColumnTitle Gtk.:= title]
--    Gtk.treeViewAppendColumn listView newCol
--    Gtk.treeViewColumnPackStart newCol renderer True
--    Gtk.cellLayoutSetAttributes newCol renderer listStore value2attributes

-- | Same as 'addColumnToTreeView'. This function can be called without a complete 'TreeViewItem'.
addColumnToTreeView' :: Gtk.CellRendererClass r =>
    (Gtk.ListStore a, Gtk.TreeView)
    -> r
    -> String
    -> (a -> [Gtk.AttrOp r])
    -> IO ()
addColumnToTreeView' (listStore, listView) renderer title value2attributes = do
    newCol <- Gtk.treeViewColumnNew
    Gtk.set newCol [Gtk.treeViewColumnTitle Gtk.:= title]
    Gtk.treeViewAppendColumn listView newCol
    Gtk.treeViewColumnPackStart newCol renderer True
    Gtk.cellLayoutSetAttributes newCol renderer listStore value2attributes

-- | Shortcut for adding text columns to a TreeView. See 'addColumnToTreeView'.
addTextColumnToTreeView :: TreeViewItem a
    -> String -- ^ title
    -> (a -> [Gtk.AttrOp Gtk.CellRendererText]) -- ^ mapping
    -> IO ()
addTextColumnToTreeView tree title map = do
    r <- Gtk.cellRendererTextNew
    addColumnToTreeView tree r title map

-- | Shortcut for adding text columns to a TreeView. See 'addColumnToTreeView\''.
addTextColumnToTreeView' :: (Gtk.ListStore a, Gtk.TreeView)
    -> String
    -> (a -> [Gtk.AttrOp Gtk.CellRendererText])
    -> IO ()
addTextColumnToTreeView' item title map = do
    r <- Gtk.cellRendererTextNew
    addColumnToTreeView' item r title map

---------------------------
-- internal helpers
---------------------------

wrapWidget :: Gtk.GObjectClass objClass =>
     Gtk.Builder
     -> (Gtk.GObject -> objClass)
     -> String -> IO (String, objClass)
wrapWidget builder cast name = do
    hPutStrLn stderr $ " cast " ++ name
    gobj <- Gtk.builderGetObject builder cast name
    return (name, gobj)
