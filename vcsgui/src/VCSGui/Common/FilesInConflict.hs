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
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Maybe
import Paths_vcsgui(getDataFileName)

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
type TreeViewSetter = [FilePath] -- ^conflicting files
                   -> TreeView
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



showFilesInConflictGUI :: (Maybe TreeViewSetter)
        -> [FilePath]                       -- ^ conflicting files
        -> Handler                          -- ^ Handler for action resolved
        -> Wrapper.Ctx ()
showFilesInConflictGUI Nothing filesInConflict actResolvedHandler =
    showFilesInConflictGUI (Just defaultSetUpTreeView) filesInConflict actResolvedHandler
showFilesInConflictGUI (Just setUpTreeView) filesInConflict actResolvedHandler = do
    liftIO $ putStrLn "Starting gui ..."
    gui <- loadFilesInConflictGUI (setUpTreeView filesInConflict)

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
defaultSetUpTreeView conflictingFiles listView = do
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
                                Just treeIter <- treeModelGetIterFromString listStore columnId
                                value <- listStoreGetValue listStore $ listStoreIterToIndex treeIter
                                let file = (\(SCFile fp _) -> fp) value
                                --TODO set cwcd to repository root, use path for programm, store pathToCmd somewhere in the repo
                                toolResolved <- Merge.exec "kdiff3" ["file1.min", "file1.r6", "file1.r8", "-o file1"]  --TODO start tool here and read exit status
                                let resolved = case toolResolved of
                                        False -> True -- TODO ask user if conflict has been resolved
                                        True -> True


                                let newValue = (\(SCFile fp b) -> SCFile fp resolved)
                                                value
                                listStoreSetValue listStore (listStoreIterToIndex treeIter) newValue
                                return ()

        return listStore

