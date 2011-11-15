-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.FilesInConflict
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Common.FilesInConflict (
    showFilesInConflictGUI
) where

import qualified VCSWrapper.Common as Wrapper
import VCSGui.Common.Types
import qualified VCSGui.Common.GtkHelper as H
import qualified VCSGui.Common.Commit as Commit
import qualified VCSGui.Common.MergeTool as Merge
import qualified VCSGui.Common.MergeToolGUI as MergeGUI
import qualified VCSGui.Common.ConflictsResolved as ConflictsResolvedGUI
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Paths_vcsgui(getDataFileName)

--
-- glade path and object accessors
--

getGladepath = getDataFileName "guiCommonFilesInConflict.glade"
accessorWindowFilesInConflict = "windowFilesInConflict"
accessorTreeViewFiles = "treeViewFiles"
accessorActResolved = "actResolved"
accessorActCancel = "actCancel"
accessorActBrowsePath = "actBrowsePath"
accessorEntPath = "entPath"
accessorActTxtViewMsg = "txtViewMsg"

--
-- types
--

-- handler being called after all files have been resolved and resolved button is pressed
type Handler = Wrapper.Ctx()

-- fn to set listStore model for treeview
type TreeViewSetter = (Maybe FilePath) -- ^ Maybe cwd
                   -> [FilePath] -- ^ conflicting files
                   -> (FilePath -> Wrapper.Ctx [FilePath]) -- ^ fn receiving a path to a conflicting file and returning all conflicting files involved in the conflict (max 4)
                   -> (FilePath -> Wrapper.Ctx ())       -- ^ fn to mark files as resolved in VCS
                   -> (Either Merge.MergeTool Merge.MergeToolSetter) -- ^ either a mergetool or fn to set one
                   -> TreeView -- ^ the treeview to set the model to
                   -> Wrapper.Ctx (ListStore SCFile)

-- GUI storing accessible elements
data GUI = GUI {
    windowFilesInConflict :: H.WindowItem
    , treeViewFiles :: H.TreeViewItem SCFile
    , actResolved :: H.ActionItem
    , actCancel :: H.ActionItem
    , actBrowsePath :: H.ActionItem
    , entPath :: H.TextEntryItem
    , txtViewMsg :: H.TextViewItem
}

--model for treestore
data SCFile = SCFile {
    filePath :: FilePath
    , isResolved :: Bool
    }
    deriving (Show)


showFilesInConflictGUI :: (Maybe TreeViewSetter) -- ^ fn to set listStore model for treeview, Nothing for default
        -> [FilePath]                            -- ^ conflicting files
        -> (FilePath -> Wrapper.Ctx [FilePath]) -- ^ fn receiving a path to a conflicting file and returning all conflicting files involved in the conflict (max 4)
        -> (FilePath -> Wrapper.Ctx ())       -- ^ fn to mark files as resolved in VCS
        -> (Either Merge.MergeTool Merge.MergeToolSetter)    -- ^ either a mergetool or fn to set one
        -> Handler                               -- ^ handler for action resolved
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
                                liftIO $ H.set (entPath gui) path
                                return Nothing
                            Right setter -> return $ Just setter

    -- connect actions
    liftIO $ H.registerClose $ windowFilesInConflict gui
    liftIO $ H.registerCloseAction (actCancel gui) (windowFilesInConflict gui)
    config <- ask
    liftIO $ on (H.getItem (actResolved gui)) actionActivated $ do
                                        --TODO check if all files have been resolved
                                        Wrapper.runVcs config $ actResolvedHandler
                                        H.closeWin (windowFilesInConflict gui)
    liftIO $ on (H.getItem (actBrowsePath gui)) actionActivated $ do
            mbPath <- showFolderChooserDialog "Select executable" (H.getItem $ windowFilesInConflict gui) FileChooserActionOpen
            case mbPath of
                Nothing -> return ()
                Just path -> do
                    -- update gui
                    H.set (entPath gui) path
                    -- call setter
                    case mbMergeToolSetter of
                        Nothing -> return ()
                        Just setter -> setter (Merge.MergeTool path)
                    return ()
    -- present window
    liftIO $ widgetShowAll $ H.getItem $ windowFilesInConflict gui

    return ()

loadGUI :: (TreeView
                    -> Wrapper.Ctx (ListStore SCFile))   -- ^ fn to set listStore model for treeview
                -> Wrapper.Ctx GUI
loadGUI setUpTreeView = do
                gladepath <- liftIO getGladepath
                builder <- liftIO $ H.openGladeFile gladepath

                win <- liftIO $ H.getWindowFromGlade builder accessorWindowFilesInConflict
                treeViewFiles <- getTreeViewFromGladeCustomStore builder accessorTreeViewFiles setUpTreeView
                actResolved <- liftIO $  H.getActionFromGlade builder accessorActResolved
                actCancel <- liftIO $  H.getActionFromGlade builder accessorActCancel
                actBrowsePath <- liftIO $   H.getActionFromGlade builder accessorActBrowsePath
                entPath <-  liftIO $  H.getTextEntryFromGlade builder accessorEntPath
                txtViewMsg <- liftIO $  H.getTextViewFromGlade builder accessorActTxtViewMsg
                return $ GUI win treeViewFiles actResolved actCancel actBrowsePath entPath txtViewMsg

defaultSetUpTreeView :: TreeViewSetter
defaultSetUpTreeView mbcwd conflictingFiles filesToResolveGetter resolveMarker eMergeToolSetter listView = do
    config <- ask
    liftIO $ do
        -- create model
        listStore <- listStoreNew [
                (SCFile      fileName
                             (False))
                | fileName <- conflictingFiles]
        treeViewSetModel listView listStore

        let treeViewItem = (listStore, listView)

        renderer <- cellRendererTextNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "File"
                               $ \scf -> [cellText := filePath scf]

        renderer <- cellRendererToggleNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Resolved"
                               $ \scf -> [cellToggleActive := isResolved scf]

        -- connect select action
        on renderer cellToggled $ \columnId -> do
                                --TODO only call tool if button is not checked, move this code to being called if a click on row is received
                                let callTool' = (\path -> Wrapper.runVcs config $ callTool columnId listStore path)


                                return ()


        return listStore
        where
            callTool columnId listStore pathToTool = do
                        config <- ask
                        Just treeIter <- liftIO $ treeModelGetIterFromString listStore columnId
                        value <- liftIO $ listStoreGetValue listStore $ listStoreIterToIndex treeIter
                        filesToResolve <- filesToResolveGetter $ filePath value
                        resolvedByTool <- liftIO $ Merge.exec mbcwd pathToTool filesToResolve
                        let setResolved' = setResolved listStore treeIter value
                        case resolvedByTool of
                                    False -> ConflictsResolvedGUI.showConflictsResolvedGUI
                                                (\resolved -> setResolved' resolved)
                                    True -> setResolved listStore treeIter value True
                        return()
            setResolved listStore treeIter oldValue isResolved = do
                        let fp = filePath oldValue
                        case isResolved of
                            False -> return ()
                            True -> resolveMarker fp
                        let newValue = (\(SCFile fp b) -> SCFile fp isResolved)
                                        oldValue
                        liftIO $ listStoreSetValue listStore (listStoreIterToIndex treeIter) newValue
                        return ()

----
---- HELPERS
----

getTreeViewFromGladeCustomStore :: Builder
                        -> String
                        -> (TreeView -> Wrapper.Ctx (ListStore SCFile)) -- ^ fn defining how to setup the liststore
                        -> Wrapper.Ctx (H.TreeViewItem SCFile)
getTreeViewFromGladeCustomStore builder name setupListStore = do
    (_, tView) <- liftIO $ wrapWidget builder castToTreeView name
    store <- setupListStore tView
    let getter = getFromListStore (store, tView)
        setter = setToListStore (store, tView)
    return (name, (store, tView), (getter, setter))

---
--- same as gtkhelper, but avoiding exposing it
---
wrapWidget :: GObjectClass objClass =>
     Builder
     -> (GObject -> objClass)
     -> String -> IO (String, objClass)
wrapWidget builder cast name = do
    putStrLn $ " cast " ++ name
    gobj <- builderGetObject builder cast name
    return (name, gobj)

getFromListStore :: (ListStore a, TreeView)
    -> IO (Maybe [a])
getFromListStore (store, _) = do
    list <- listStoreToList store
    if null list
        then return Nothing
        else return $ Just list

setToListStore :: (ListStore a, TreeView)
    -> [a]
    -> IO ()
setToListStore (store, view) newList = do
    listStoreClear store
    mapM_ (listStoreAppend store) newList
    return ()

-- HELPER

-- | shows a dialog to choose a folder, returns Just FilePath to folder if succesfull, Nothing if cancelled
showFolderChooserDialog :: String -- ^ title of the window
    -> Window -- ^ parent window
    -> FileChooserAction
    -> IO (Maybe FilePath)
showFolderChooserDialog title parent fcAction = do
    dialog <- fileChooserDialogNew (Just title) (Just parent) fcAction [("Cancel", ResponseCancel), ("Select", ResponseAccept)]
    response <- dialogRun dialog
    case response of
        ResponseCancel      -> widgetDestroy dialog >> return Nothing
        ResponseDeleteEvent -> widgetDestroy dialog >> return Nothing
        ResponseAccept      -> do
            f <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return f


