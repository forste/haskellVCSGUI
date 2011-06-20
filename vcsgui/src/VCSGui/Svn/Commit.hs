-----------------------------------------------------------------------------
--
-- Module      :  Main
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
{-# LANGUAGE ScopedTypeVariables #-}
module VCSGui.Svn.Commit (
    showCommitGUI
) where

import qualified VCSGui.Common.Commit as C
import VCSGui.Common.Types
import Graphics.UI.Gtk
import qualified VCSWrapper.Svn as Svn
import Control.Monad.Trans(liftIO)
import VCSWrapper.Common
import qualified VCSGui.Common.GtkHelper as H

showCommitGUI :: Svn.Ctx()
showCommitGUI = C.showCommitGUI setUpTreeView okCallback

okCallback :: String        -- ^ commit message
            -> [FilePath]   -- ^ selected files
            -> [C.Option]   -- ^ TODO options
            -> Ctx ()
okCallback msg filesToCommit _ =  do
                                liftIO $ putStrLn $ "Files to commit (adding them also): "++show filesToCommit
                                liftIO $ putStrLn $ "Commit message: "++msg
                                Svn.add [] filesToCommit
                                Svn.commit filesToCommit msg []
                                return ()

setUpTreeView :: TreeView -> Svn.Ctx (ListStore C.SCFile)
setUpTreeView listView = do
    -- get status
    repoStatus <- Svn.status []

    liftIO $ do
        -- create model
        listStore <- listStoreNew [
                (C.SVNSCFile (ctxSelect (Svn.modification status))
                             (Svn.filePath status)
                             (show (Svn.modification status))
                             (Svn.isLocked status))
                | status <- repoStatus]
        treeViewSetModel listView listStore

        let treeViewItem = (listStore, listView)

        renderer <- cellRendererToggleNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               ""
                               $ \scf -> [cellToggleActive := C.selected scf]

        -- connect select action
        on renderer cellToggled $ \columnId -> do
                                Just treeIter <- treeModelGetIterFromString listStore columnId
                                value <- listStoreGetValue listStore $ listStoreIterToIndex treeIter
                                let newValue = (\(C.SVNSCFile bool fp s l) -> C.SVNSCFile (not bool) fp s l)
                                                value
                                listStoreSetValue listStore (listStoreIterToIndex treeIter) newValue
                                return ()

        renderer <- cellRendererTextNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Files to commit"
                               $ \scf -> [cellText := C.filePath scf]

        renderer <- cellRendererTextNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Status"
                               $ \scf -> [cellText := C.status scf]

        renderer <- cellRendererToggleNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Locked"
                               $ \scf -> [cellToggleActive := C.isLocked scf]
        return listStore
    where
        ctxSelect status =  status == Svn.Added || status == Svn.Deleted || status==Svn.Modified ||
                            status == Svn.Replaced
