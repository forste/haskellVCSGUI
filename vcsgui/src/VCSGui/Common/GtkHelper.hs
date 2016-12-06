{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
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

import System.Directory
import Control.Monad.Trans(liftIO)
import System.IO (hPutStrLn, stderr)
import VCSGui.Common.Helpers (emptyTextToNothing)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified GI.Gtk.Objects.Window as Gtk (Window(..))
import qualified GI.Gtk.Objects.Action as Gtk
       (onActionActivate, Action(..))
import qualified GI.Gtk.Objects.Label as Gtk
       (labelSetText, Label(..))
import qualified GI.Gtk.Objects.Entry as Gtk
       (entrySetText, entryGetText, Entry(..))
import qualified GI.Gtk.Objects.ComboBox as Gtk (ComboBox(..))
import qualified GI.Gtk.Objects.TextView as Gtk
       (textViewGetBuffer, TextView(..))
import qualified Data.GI.Gtk.ModelView.SeqStore as Gtk
       (seqStoreToList, seqStoreNew, seqStoreAppend, seqStoreClear,
        SeqStore(..))
import qualified GI.Gtk.Objects.TreeView as Gtk
       (treeViewAppendColumn, treeViewSetModel, TreeView(..))
import qualified GI.Gtk.Objects.CheckButton as Gtk
       (CheckButton(..))
import qualified GI.Gtk.Objects.Button as Gtk
       (buttonSetLabel, buttonGetLabel, Button(..))
import qualified GI.Gtk.Objects.Builder as Gtk
       (builderGetObject, builderAddFromFile, builderNew, Builder(..))
import qualified Data.GI.Gtk.ComboBox as Gtk
       (comboBoxGetModelText, comboBoxGetActiveText, comboBoxSetModelText)
import qualified GI.Gtk.Objects.TextBuffer as Gtk
       (textBufferGetText, textBufferGetEndIter, textBufferGetStartIter,
        textBufferSetText)
import qualified GI.Gtk.Structs.TextIter as Gtk (textIterEqual)
import qualified GI.Gtk.Objects.ToggleButton as Gtk
       (toggleButtonSetActive, toggleButtonGetActive)
import qualified GI.Gtk.Objects.Widget as Gtk
       (onWidgetDeleteEvent, widgetHide)
import qualified GI.Gtk.Functions as Gtk (mainQuit)
import qualified GI.Gtk.Objects.CellRenderer as Gtk (IsCellRenderer)
import qualified GI.Gtk.Objects.TreeViewColumn as Gtk
       (treeViewColumnPackStart, setTreeViewColumnTitle,
        treeViewColumnNew)
import qualified Data.GI.Gtk.ModelView.CellLayout as Gtk
       (cellLayoutSetDataFunction)
import qualified GI.Gtk.Objects.CellRendererText as Gtk
       (cellRendererTextNew, CellRendererText(..))
import qualified Data.GI.Base.BasicTypes as Gtk (GObject)
import Foreign.ForeignPtr (ForeignPtr)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.GI.Base.BasicTypes (ManagedPtr(..), NullToNothing(..))
import Data.Maybe (fromJust)

-- Typesynonyms
type WindowItem = (Text, Gtk.Window, ())
type ActionItem = (Text, Gtk.Action, ())
type LabelItem = (Text, Gtk.Label, (IO (Maybe Text), Text -> IO ()))
type TextEntryItem = (Text, Gtk.Entry, (IO (Maybe Text), Text -> IO ()))
type ComboBoxItem = (Text, Gtk.ComboBox, (IO (Maybe Text), [Text] -> IO ()))
type TextViewItem = (Text, Gtk.TextView, (IO (Maybe Text), Text -> IO ()))
type TreeViewItem a = (Text, (Gtk.SeqStore a, Gtk.TreeView), (IO (Maybe [a]), [a] -> IO ()))
type CheckButtonItem = (Text, Gtk.CheckButton, (IO Bool, Bool -> IO()))
type ButtonItem = (Text, Gtk.Button, (IO Text, Text -> IO()))

-- Type accessors

-- | return the name of this item (as in the gladefile)
getName :: (Text, a, b) -> Text
getName (n, _, _) = n

-- | return the Gtk object wrapped by given item
getItem :: (Text, a, b) -> a
getItem (_, item, _) = item

-- | call teh get method of an *Item
get :: (Text, a, (b, c)) -> b
get (_, _, (getter, _)) = getter

-- | call the set method of an *Item
set :: (Text, a, (b, c)) -> c
set (_,_, (_, setter)) = setter


----------------------
-- *FromGlade
----------------------

-- | Open a gladefile with a new 'Gtk.Builder'.
openGladeFile :: FilePath -- ^ Gladefile to open.
    -> IO Gtk.Builder
openGladeFile fn = do
    builder <- Gtk.builderNew
    Gtk.builderAddFromFile builder $ T.pack fn
    return builder

-- | Get a 'WindowItem' from a gladefile.
getWindowFromGlade :: Gtk.Builder
    -> Text -- ^ name of the window to get as specified in the gladefile.
    -> IO WindowItem
getWindowFromGlade builder name = do
    (a, b) <- wrapWidget builder Gtk.Window name
    return (a, b, ())

-- | Get an 'ActionItem' from a gladefile.
getActionFromGlade :: Gtk.Builder
    -> Text -- ^ name of the action to get as specified in the gladefile.
    -> IO ActionItem
getActionFromGlade builder name = do
    (a, b) <- wrapWidget builder Gtk.Action name
    return (a, b, ())

-- | Get an 'LabelItem' from a gladefile.
getLabelFromGlade :: Gtk.Builder
    -> Text -- ^ name of the label to get as specified in the gladefile.
    -> IO LabelItem
getLabelFromGlade builder name = do
    (_, entry) <- wrapWidget builder Gtk.Label name
    let getter = error "don't call get on a gtk label!" :: IO (Maybe Text)
        setter val = Gtk.labelSetText entry val :: IO ()
    return (name, entry, (getter, setter))

-- | Get a 'ButtonItem' from a gladefile.
getButtonFromGlade :: Gtk.Builder
    -> Text -- ^ name of the button to get as specified in the gladefile.
    -> IO ButtonItem
getButtonFromGlade builder name = do
    (_,btn) <- wrapWidget builder Gtk.Button name
    let getter = Gtk.buttonGetLabel btn :: IO Text
        setter val = Gtk.buttonSetLabel btn val
    return (name, btn, (getter,setter))

-- | Get a 'TextEntryItem' from a gladefile.
getTextEntryFromGlade :: Gtk.Builder
    -> Text -- ^ name of the text entry to get as specified in the gladefile.
    -> IO TextEntryItem
getTextEntryFromGlade builder name = do
    (_, entry) <- wrapWidget builder Gtk.Entry name
    let getter = fmap emptyTextToNothing $ Gtk.entryGetText entry :: IO (Maybe Text)
        setter val = Gtk.entrySetText entry val :: IO ()
    return (name, entry, (getter, setter))

-- | Get a 'ComboBoxItem' from a gladefile.
getComboBoxFromGlade :: Gtk.Builder
                    -> Text -- ^ name of the combo box to get as specified in the gladefile.
                    -> IO ComboBoxItem
getComboBoxFromGlade builder name = do
    (_, combo) <- wrapWidget builder Gtk.ComboBox name
    Gtk.comboBoxSetModelText combo
    let getter = Gtk.comboBoxGetActiveText combo  :: IO (Maybe Text) -- get selected text
        setter entries = do -- fill with new entries
            store <- Gtk.comboBoxGetModelText combo
            Gtk.seqStoreClear store
            mapM_ (Gtk.seqStoreAppend store) entries
            return ()
    return (name, combo, (getter, setter))

-- | Get a 'TextViewItem' from a gladefile.
getTextViewFromGlade :: Gtk.Builder
    -> Text -- ^ name of the text view to get as specified in the gladefile.
    -> IO TextViewItem
getTextViewFromGlade builder name =  do
        (_, entry)  <- wrapWidget builder Gtk.TextView name
        buffer <- Gtk.textViewGetBuffer entry
        let getter = getLongText buffer :: IO (Maybe Text)
            setter = (\text -> Gtk.textBufferSetText buffer text (-1)) :: Text -> IO ()
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
    -> Text -- ^ name of the check button to get as specified in the gladefile.
    -> IO CheckButtonItem
getCheckButtonFromGlade builder name = do
        (_,bt) <- wrapWidget builder Gtk.CheckButton name
        let getter = Gtk.toggleButtonGetActive bt
            setter = (\bool -> Gtk.toggleButtonSetActive bt bool) :: Bool -> IO()
        return (name,bt, (getter,setter))

---------------------------------
-- TreeView
---------------------------------

-- | Get a 'TreeViewItem' from a gladefile.
getTreeViewFromGlade :: Gtk.Builder
    -> Text -- ^ name of the tree view to get as specified in the gladefile.
    -> [a] -- ^ Content of the new tree view.
    -> IO (TreeViewItem a)
getTreeViewFromGlade builder name rows = do
    (_, tView) <- wrapWidget builder Gtk.TreeView name
    entry@(store, treeView) <- createStoreForTreeView tView rows
    let getter = getFromSeqStore entry
        setter = setToSeqStore entry
    return (name, (store, treeView), (getter, setter))

-- | Get a 'TreeViewItem' from a gladefile.
getTreeViewFromGladeCustomStore :: Gtk.Builder
                        -> Text -- ^ name of the tree view to get as specified in the gladefile.
                        -> (Gtk.TreeView -> IO (Gtk.SeqStore a)) -- ^ fn defining how to setup the liststore
                        -> IO (TreeViewItem a)
getTreeViewFromGladeCustomStore builder name setupSeqStore = do
    (_, tView) <- wrapWidget builder Gtk.TreeView name
    store <- setupSeqStore tView
    Gtk.treeViewSetModel tView (Just store)
    let getter = getFromSeqStore (store, tView)
        setter = setToSeqStore (store, tView)
    return (name, (store, tView), (getter, setter))

-- | Create a new 'Gtk.SeqStore' for a 'Gtk.TreeView'.
createStoreForTreeView :: Gtk.TreeView -- ^ The created list store will be set the model for this TreeView.
    -> [a] -- ^ Content of the new store.
    -> IO (Gtk.SeqStore a, Gtk.TreeView)
createStoreForTreeView listView rows = do
    seqStore <- Gtk.seqStoreNew rows
    Gtk.treeViewSetModel listView (Just seqStore)
    return (seqStore, listView)

-- | Get the content of a SeqStore.
getFromSeqStore :: (Gtk.SeqStore a, Gtk.TreeView)
    -> IO (Maybe [a]) -- ^ Nothing if the SeqStore is empty.
getFromSeqStore (store, _) = do
    list <- Gtk.seqStoreToList store
    if null list
        then return Nothing
        else return $ Just list

-- | Set the content of a SeqStore.
setToSeqStore :: (Gtk.SeqStore a, Gtk.TreeView)
    -> [a] -- ^ New content of the SeqStore.
    -> IO ()
setToSeqStore (store, view) newList = do
    Gtk.seqStoreClear store
    mapM_ (Gtk.seqStoreAppend store) newList
    return ()

--
-- Various helpers
--

-- | Close a window.
closeWin :: WindowItem -> IO ()
closeWin win = (Gtk.widgetHide (getItem win))

-- | Close a window if 'Gtk.deleteEvent' occurs on this 'WindowItem'.
registerClose :: WindowItem -> IO ()
registerClose win = Gtk.onWidgetDeleteEvent (getItem win) (\_ -> liftIO (closeWin win) >> return False) >> return ()

-- | Close a window if the specified action occurs on this 'WindowItem'.
registerCloseAction :: ActionItem -> WindowItem -> IO ()
registerCloseAction act win = Gtk.onActionActivate (getItem act) (liftIO (closeWin win)) >> return ()

-- | Call 'Gtk.mainQuit' if 'Gtk.deleteEvent' occurs on this 'WindowItem'.
registerQuit :: WindowItem -> IO ()
registerQuit win = Gtk.onWidgetDeleteEvent (getItem win) (\_ -> liftIO $ Gtk.mainQuit >> return False) >> return ()

-- | Call 'Gtk.mainQuit' if the specified action occurs on this 'WindowItem'.
registerQuitAction :: ActionItem -> IO ()
registerQuitAction act = Gtk.onActionActivate (getItem act) (liftIO (Gtk.mainQuit)) >> return ()

-- TODO fun argument is not used. what was the purpos of this function?
-- | same as 'registerQuitAction' since second argument is ignored (?)
registerQuitWithCustomFun :: WindowItem
                -> IO () -- ^ custom fun
                -> IO ()
registerQuitWithCustomFun win fun = Gtk.onWidgetDeleteEvent (getItem win) (\_ -> liftIO $ Gtk.mainQuit >> return False) >> return ()

-- | Add a column to given SeqStore and TreeView using a mapping.
-- The mapping consists of a CellRenderer, the title and a function, that maps each row to attributes of the column
addColumnToTreeView :: Gtk.IsCellRenderer r =>
    TreeViewItem a
    -> r -- ^ CellRenderer
    -> Text -- ^ title
    -> (r -> a -> IO ()) -- ^ mapping
    -> IO ()
addColumnToTreeView (_, item, _) = do
    addColumnToTreeView' item
--    newCol <- Gtk.treeViewColumnNew
--    Gtk.set newCol [Gtk.treeViewColumnTitle Gtk.:= title]
--    Gtk.treeViewAppendColumn listView newCol
--    Gtk.treeViewColumnPackStart newCol renderer True
--    Gtk.cellLayoutSetAttributes newCol renderer seqStore value2attributes

-- | Same as 'addColumnToTreeView'. This function can be called without a complete 'TreeViewItem'.
addColumnToTreeView' :: Gtk.IsCellRenderer r =>
    (Gtk.SeqStore a, Gtk.TreeView)
    -> r
    -> Text
    -> (r -> a -> IO ())
    -> IO ()
addColumnToTreeView' (seqStore, listView) renderer title value2attributes = do
    newCol <- Gtk.treeViewColumnNew
    Gtk.setTreeViewColumnTitle newCol title
    Gtk.treeViewAppendColumn listView newCol
    Gtk.treeViewColumnPackStart newCol renderer True
    Gtk.cellLayoutSetDataFunction newCol renderer seqStore (value2attributes renderer)

-- | Shortcut for adding text columns to a TreeView. See 'addColumnToTreeView'.
addTextColumnToTreeView :: TreeViewItem a
    -> Text -- ^ title
    -> (Gtk.CellRendererText -> a -> IO ()) -- ^ mapping
    -> IO ()
addTextColumnToTreeView tree title map = do
    r <- Gtk.cellRendererTextNew
    addColumnToTreeView tree r title map

-- | Shortcut for adding text columns to a TreeView. See 'addColumnToTreeView\''.
addTextColumnToTreeView' :: (Gtk.SeqStore a, Gtk.TreeView)
    -> Text
    -> (Gtk.CellRendererText -> a -> IO ())
    -> IO ()
addTextColumnToTreeView' item title map = do
    r <- Gtk.cellRendererTextNew
    addColumnToTreeView' item r title map

---------------------------
-- internal helpers
---------------------------

wrapWidget :: Gtk.GObject objClass =>
     Gtk.Builder
     -> (ManagedPtr objClass -> objClass)
     -> Text -> IO (Text, objClass)
wrapWidget builder constructor name = do
    hPutStrLn stderr $ " cast " ++ T.unpack name
    gobj <- nullToNothing (Gtk.builderGetObject builder name) >>= unsafeCastTo constructor . fromJust
    return (name, gobj)


