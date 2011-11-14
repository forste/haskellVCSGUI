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
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Maybe
import Paths_vcsgui(getDataFileName)
import Data.List.Utils(startswith)
import Monad (filterM)
import System.Directory(doesFileExist, getDirectoryContents)
import System.FilePath

--
-- glade path and object accessors
--

getGladepath = getDataFileName "guiCommonFilesInConflict.glade"
accessorWindowFilesInConflict = "windowFilesInConflict"
accessorTreeViewFiles = "treeViewFiles"
accessorActResolved = "actResolved"
accessorActCancel = "actCancel"
accessorActTxtViewMsg = "txtViewMsg"

--
-- types
--

-- hand
type Handler = Commit.ShowCommitGUI

-- fn to set listStore model for treeview
type TreeViewSetter = (Maybe FilePath) -- ^ Maybe cwd
                   -> [FilePath] -- ^ conflicting files
                   -> (Either Merge.MergeTool Merge.MergeToolSetter) -- ^ either a mergetool or fn to set one
                   -> TreeView -- ^ the treeview to set the model to
                   -> Wrapper.Ctx (ListStore SCFile)

data FilesInConflictGUI = LogGUI {
    windowFilesInConflict :: H.WindowItem
    , treeViewFiles :: H.TreeViewItem SCFile
    , actResolved :: H.ActionItem
    , actCancel :: H.ActionItem
    , txtViewMsg :: H.TextViewItem
}

--model for treestore
data SCFile = SCFile FilePath Bool
    deriving (Show)

filePath :: SCFile -> FilePath
filePath (SCFile fp _) = fp

isResolved :: SCFile -> Bool
isResolved (SCFile _ r) = r

type Option = String



showFilesInConflictGUI :: (Maybe TreeViewSetter) -- ^ fn to set listStore model for treeview, Nothing for default
        -> [FilePath]                            -- ^ conflicting files
        -> (Either Merge.MergeTool Merge.MergeToolSetter)    -- ^ either a mergetool or fn to set one
        -> Handler                               -- ^ handler for action resolved
        -> Wrapper.Ctx ()
showFilesInConflictGUI Nothing f e a =
    showFilesInConflictGUI (Just defaultSetUpTreeView) f e a
showFilesInConflictGUI (Just setUpTreeView) filesInConflict eMergeToolSetter actResolvedHandler = do
    liftIO $ putStrLn "Starting files in conflict gui ..."
    config <- ask
    let cwd = (Wrapper.configCwd config)
    gui <- loadFilesInConflictGUI (setUpTreeView cwd filesInConflict eMergeToolSetter)

    -- connect actions
    liftIO $ H.registerClose $ windowFilesInConflict gui
    liftIO $ H.registerCloseAction (actCancel gui) (windowFilesInConflict gui)
    config <- ask
    liftIO $ on (H.getItem (actResolved gui)) actionActivated $ do
                                        Wrapper.runVcs config $ actResolvedHandler
                                        H.closeWin (windowFilesInConflict gui)

    -- present window
    liftIO $ widgetShowAll $ H.getItem $ windowFilesInConflict gui

    return ()



loadFilesInConflictGUI :: (TreeView
                    -> Wrapper.Ctx (ListStore SCFile))   -- ^ fn to set listStore model for treeview
                -> Wrapper.Ctx FilesInConflictGUI
loadFilesInConflictGUI setUpTreeView = do
                gladepath <- liftIO getGladepath
                builder <- liftIO $ H.openGladeFile gladepath

                win <- liftIO $ H.getWindowFromGlade builder accessorWindowFilesInConflict
                treeViewFiles <- getTreeViewFromGladeCustomStore builder accessorTreeViewFiles setUpTreeView
                actResolved <- liftIO $  H.getActionFromGlade builder accessorActResolved
                actCancel <- liftIO $  H.getActionFromGlade builder accessorActCancel
                txtViewMsg <- liftIO $  H.getTextViewFromGlade builder accessorActTxtViewMsg
                return $ LogGUI win treeViewFiles actResolved actCancel txtViewMsg
----
---- HELPERS
----

--getSelectedFiles :: ListStore SCFile -> IO [FilePath]
--getSelectedFiles listStore = do
--            listedFiles <- listStoreToList listStore
--            let selectedFiles = map (\scf -> filePath scf )
--                                $ filter (\scf -> selected scf) listedFiles
--            return (selectedFiles)

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

defaultSetUpTreeView :: TreeViewSetter
defaultSetUpTreeView mbcwd conflictingFiles eMergeToolSetter listView = do
    config <- ask
    let cwd = Wrapper.configCwd config
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
                               "Files"
                               $ \scf -> [cellText := filePath scf]

        renderer <- cellRendererToggleNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Resolve"
                               $ \scf -> [cellToggleActive := isResolved scf]

        -- connect select action
        on renderer cellToggled $ \columnId -> do

                                mergeTool <- case eMergeToolSetter of
                                    Left (Merge.MergeTool path) -> callTool path columnId listStore cwd
                                    Right setter -> do
                                        MergeGUI.showMergeToolGUI (\(Merge.MergeTool path) -> do
                                                                                                setter (Merge.MergeTool path)
                                                                                                callTool path columnId listStore cwd)
                                return ()


        return listStore
        where
            callTool pathToTool columnId listStore cwd = do
                        Just treeIter <- treeModelGetIterFromString listStore columnId
                        value <- listStoreGetValue listStore $ listStoreIterToIndex treeIter
                        let file = combine (fromMaybe "" cwd) $ (\(SCFile fp _) -> fp) value
                        putStrLn $ "file" ++ file

                        let (fileD,fileN) = splitFileName file
                        putStrLn $ "(fileD, fileN) (" ++ fileD ++ ", " ++ fileN ++ ")"

                        content <- getDirectoryContents fileD
                        putStrLn $ "content" ++ (show content)
                        let contentWithD = map (\cN -> combine fileD cN) content
                        putStrLn $ "contentWithD" ++ (show contentWithD)
                        files <- filterM doesFileExist contentWithD
                        putStrLn $ "files" ++ (show files)
                        let filesToResolve = [f | f <- files, (startswith (file++".r") f) || (f == (file++".mine"))]++[file]
                        putStrLn $ "Files to Resolve " ++ (show filesToResolve)

                        resolvedByTool <- Merge.exec mbcwd pathToTool filesToResolve
                        let resolved = case resolvedByTool of
                                False -> True -- TODO ask user if conflict has been resolved
                                True -> True

                        let newValue = (\(SCFile fp b) -> SCFile fp resolved)
                                        value
                        listStoreSetValue listStore (listStoreIterToIndex treeIter) newValue
                        return ()


