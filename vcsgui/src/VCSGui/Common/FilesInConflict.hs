{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.FilesInConflict
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides a GUI to show and resolve conflicts in a VCS.
--
-----------------------------------------------------------------------------

module VCSGui.Common.FilesInConflict (
    showFilesInConflictGUI
) where

import qualified VCSWrapper.Common as Wrapper
import qualified VCSGui.Common.GtkHelper as H
import qualified VCSGui.Common.Commit as Commit
import qualified VCSGui.Common.MergeTool as Merge
import qualified VCSGui.Common.Process as Process
import qualified VCSGui.Common.ConflictsResolved as ConflictsResolvedGUI
import qualified VCSGui.Common.Error as Error
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Paths_vcsgui(getDataFileName)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import GI.Gtk.Objects.TreeView (treeViewSetModel, TreeView(..))
import GI.Gtk.Objects.Action (onActionActivate)
import GI.Gtk.Enums (ResponseType(..), FileChooserAction(..))
import GI.Gtk.Objects.Widget (widgetDestroy, widgetShowAll)
import GI.Gtk.Objects.CellRendererText (cellRendererTextNew)
import GI.Gtk.Objects.CellRendererToggle
       (onCellRendererToggleToggled, cellRendererToggleNew)
import GI.Gtk.Interfaces.TreeModel (treeModelGetIterFromString)
import GI.Gtk.Objects.Builder (builderGetObject, Builder(..))
import Data.GI.Base.BasicTypes
       (ManagedPtr(..), NullToNothing(..), GObject)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreAppend, seqStoreClear, seqStoreToList,
        seqStoreSetValue, seqStoreIterToIndex, seqStoreGetValue,
        seqStoreNew, SeqStore(..))
import GI.Gtk.Objects.Window
       (setWindowTransientFor, setWindowTitle, Window(..))
import Data.GI.Base (new', nullToNothing)
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Objects.Dialog (dialogRun, dialogAddButton)
import GI.Gtk.Interfaces.FileChooser
       (fileChooserGetFilename, setFileChooserAction)
import Data.Maybe (fromJust)
import GI.Gtk
       (setCellRendererToggleActive, setCellRendererTextText)

--
-- glade path and object accessors
--

getGladepath = getDataFileName "data/guiCommonFilesInConflict.glade"
accessorWindowFilesInConflict = "windowFilesInConflict"
accessorTreeViewFiles = "treeViewFiles"
accessorActResolved = "actResolved"
accessorActCancel = "actCancel"
accessorActBrowsePath = "actBrowsePath"
accessorEntPath = "entPath"

--
-- types
--

-- | Handler being called after all files have been resolved and resolved button is pressed
type Handler = Wrapper.Ctx()

-- fn to set seqStore model for treeview
type TreeViewSetter = (Maybe FilePath) -- ^ Maybe cwd
                   -> [FilePath] -- ^ conflicting files
                   -> (FilePath -> Wrapper.Ctx [FilePath]) -- ^ fn receiving a path to a conflicting file and returning all conflicting files involved in the conflict (max 4)
                   -> (FilePath -> Wrapper.Ctx ())       -- ^ fn to mark files as resolved in VCS
                   -> (Either Merge.MergeTool Merge.MergeToolSetter) -- ^ either a mergetool or fn to set one
                   -> H.TextEntryItem   -- ^ the entry to get the path to the mergetool from
                   -> TreeView -- ^ the treeview to set the model to
                   -> Wrapper.Ctx (SeqStore SCFile)

-- GUI storing accessible elements
data GUI = GUI {
    windowFilesInConflict :: H.WindowItem
    , treeViewFiles :: H.TreeViewItem SCFile
    , actResolved :: H.ActionItem
    , actCancel :: H.ActionItem
    , actBrowsePath :: H.ActionItem
    , entPath :: H.TextEntryItem
}

--model for treestore
data SCFile = SCFile {
    filePath :: FilePath
    , isResolved :: Bool
    }
    deriving (Show)


-- | Shows a GUI showing conflicting files and providing means to resolve the conflicts.
showFilesInConflictGUI :: (Maybe TreeViewSetter) -- ^ fn to set seqStore model for treeview, Nothing for default
        -> [FilePath]                            -- ^ conflicting files
        -> (FilePath -> Wrapper.Ctx [FilePath]) -- ^ fn receiving a path to a conflicting file and returning all conflicting files involved in the conflict (max 4)
        -> (FilePath -> Wrapper.Ctx ())       -- ^ fn to mark files as resolved in VCS
        -> (Either Merge.MergeTool Merge.MergeToolSetter)    -- ^ 'MergeTool' is used for any possible conflicts. If not present user will be asked to provide 'MergeTool' on conflicts. 'MergeToolSetter' will be called for response.
        -> Handler                               -- ^ 'Handler' for action resolved
        -> Wrapper.Ctx ()
showFilesInConflictGUI Nothing f g m e a =
    showFilesInConflictGUI (Just defaultSetUpTreeView) f g m e a
showFilesInConflictGUI (Just setUpTreeView) filesInConflict filesToResolveGetter resolveMarker eMergeToolSetter actResolvedHandler = do
    liftIO $ putStrLn "Starting files in conflict gui ..."
    config <- ask
    let cwd = (Wrapper.configCwd config)
    gui <- loadGUI $ setUpTreeView cwd filesInConflict filesToResolveGetter resolveMarker eMergeToolSetter
    mbMergeToolSetter <- case eMergeToolSetter of
                            Left (Merge.MergeTool path) -> do
                                liftIO $ H.set (entPath gui) $ T.pack path
                                return Nothing
                            Right setter -> return $ Just setter

    -- connect actions
    liftIO $ H.registerClose $ windowFilesInConflict gui
    liftIO $ H.registerCloseAction (actCancel gui) (windowFilesInConflict gui)
    config <- ask
    liftIO $ onActionActivate (H.getItem (actResolved gui)) $ do
                                        --TODO check if all files have been resolved
                                        Wrapper.runVcs config $ actResolvedHandler
                                        H.closeWin (windowFilesInConflict gui)
    liftIO $ onActionActivate (H.getItem (actBrowsePath gui)) $ do
            mbPath <- showFolderChooserDialog "Select executable" (H.getItem $ windowFilesInConflict gui) FileChooserActionOpen
            case mbPath of
                Nothing -> return ()
                Just path -> do
                    -- update gui
                    H.set (entPath gui) $ T.pack path
                    -- call setter
                    case mbMergeToolSetter of
                        Nothing -> return ()
                        Just setter -> setter (Merge.MergeTool path)
                    return ()
    -- present window
    liftIO $ widgetShowAll $ H.getItem $ windowFilesInConflict gui

    return ()

loadGUI :: (H.TextEntryItem -> TreeView -> Wrapper.Ctx (SeqStore SCFile))
        -- ^ (The entry to get the path to the mergetool from. , treeview to setup, fn to set seqStore model for treeview
        -> Wrapper.Ctx GUI
loadGUI setUpTreeView = do
                gladepath <- liftIO getGladepath
                builder <- liftIO $ H.openGladeFile gladepath

                win <- liftIO $ H.getWindowFromGlade builder accessorWindowFilesInConflict
                entPath <-  liftIO $  H.getTextEntryFromGlade builder accessorEntPath
                treeViewFiles <- getTreeViewFromGladeCustomStore builder accessorTreeViewFiles (setUpTreeView entPath)
                actResolved <- liftIO $  H.getActionFromGlade builder accessorActResolved
                actCancel <- liftIO $  H.getActionFromGlade builder accessorActCancel
                actBrowsePath <- liftIO $   H.getActionFromGlade builder accessorActBrowsePath
                return $ GUI win treeViewFiles actResolved actCancel actBrowsePath entPath

defaultSetUpTreeView :: TreeViewSetter
defaultSetUpTreeView mbcwd conflictingFiles filesToResolveGetter resolveMarker eMergeToolSetter entPath listView = do
    config <- ask
    liftIO $ do
        -- create model
        seqStore <- seqStoreNew [
                (SCFile      fileName
                             (False))
                | fileName <- conflictingFiles]
        treeViewSetModel listView (Just seqStore)

        let treeViewItem = (seqStore, listView)

        renderer <- cellRendererTextNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "File"
                               $ \cell scf -> setCellRendererTextText cell . T.pack $ filePath scf

        renderer <- cellRendererToggleNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Resolved"
                               $ \cell scf -> setCellRendererToggleActive cell $ isResolved scf

        -- connect select action
        onCellRendererToggleToggled renderer $ \(columnId :: Text) -> do
                                putStrLn $ "Checkbutton clicked at column " ++ (show columnId)
                                --TODO only call tool if button is not checked, move this code to being called if a click on row is received
                                let callTool' = (\path -> Wrapper.runVcs config $ callTool columnId seqStore path)
                                mbPath <- H.get entPath
                                case mbPath of
                                    Nothing -> Error.showErrorGUI "MergeTool not set. Set MergeTool first."
                                    Just path -> callTool' path
                                return ()


        return seqStore
        where
            callTool columnId seqStore pathToTool = do
                        config <- ask
                        (True, treeIter) <- liftIO $ treeModelGetIterFromString seqStore columnId
                        value <- liftIO $ seqStoreGetValue seqStore =<< seqStoreIterToIndex treeIter
                        filesToResolve <- filesToResolveGetter $ filePath value
                        resolvedByTool <- liftIO $ Process.exec mbcwd pathToTool $ map T.pack filesToResolve
                        let setResolved' = setResolved seqStore treeIter value
                        case resolvedByTool of
                                    False -> ConflictsResolvedGUI.showConflictsResolvedGUI
                                                (\resolved -> setResolved' resolved)
                                    True -> setResolved seqStore treeIter value True
                        return()
            setResolved seqStore treeIter oldValue isResolved = do
                        let fp = filePath oldValue
                        case isResolved of
                            False -> return ()
                            True -> resolveMarker fp
                        let newValue = (\(SCFile fp b) -> SCFile fp isResolved)
                                        oldValue
                        n <- seqStoreIterToIndex treeIter
                        liftIO $ seqStoreSetValue seqStore n newValue
                        return ()

----
---- HELPERS
----

getTreeViewFromGladeCustomStore :: Builder
                        -> Text
                        -> (TreeView -> Wrapper.Ctx (SeqStore SCFile)) -- ^ fn defining how to setup the liststore
                        -> Wrapper.Ctx (H.TreeViewItem SCFile)
getTreeViewFromGladeCustomStore builder name setupSeqStore = do
    (_, tView) <- liftIO $ wrapWidget builder TreeView name
    store <- setupSeqStore tView
    let getter = getFromSeqStore (store, tView)
        setter = setToSeqStore (store, tView)
    return (name, (store, tView), (getter, setter))

---
--- same as gtkhelper, but avoiding exposing it
---
wrapWidget :: GObject objClass =>
     Builder
     -> (ManagedPtr objClass -> objClass)
     -> Text -> IO (Text, objClass)
wrapWidget builder constructor name = do
    putStrLn $ " cast " ++ T.unpack name
    gobj <- nullToNothing (builderGetObject builder name) >>= unsafeCastTo constructor . fromJust
    return (name, gobj)

getFromSeqStore :: (SeqStore a, TreeView)
    -> IO (Maybe [a])
getFromSeqStore (store, _) = do
    list <- seqStoreToList store
    if null list
        then return Nothing
        else return $ Just list

setToSeqStore :: (SeqStore a, TreeView)
    -> [a]
    -> IO ()
setToSeqStore (store, view) newList = do
    seqStoreClear store
    mapM_ (seqStoreAppend store) newList
    return ()

-- HELPER

-- | shows a dialog to choose a folder, returns Just FilePath to folder if succesfull, Nothing if cancelled
showFolderChooserDialog :: Text -- ^ title of the window
    -> Window -- ^ parent window
    -> FileChooserAction
    -> IO (Maybe FilePath)
showFolderChooserDialog title parent fcAction = do
    dialog <- new' FileChooserDialog []
    setWindowTitle dialog title
    dialogAddButton dialog "gtk-cancel" (fromIntegral $ fromEnum ResponseTypeCancel)
    dialogAddButton dialog "Select" (fromIntegral $ fromEnum ResponseTypeAccept)
    setWindowTransientFor dialog parent
    setFileChooserAction dialog fcAction
    response <- dialogRun dialog
    case toEnum $ fromIntegral response of
        ResponseTypeCancel      -> widgetDestroy dialog >> return Nothing
        ResponseTypeDeleteEvent -> widgetDestroy dialog >> return Nothing
        ResponseTypeAccept      -> do
            f <- nullToNothing $ fileChooserGetFilename dialog
            widgetDestroy dialog
            return f


