{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides a GUI to commit to a SVN working copy.
--
-----------------------------------------------------------------------------
module VCSGui.Svn.Commit (
    showCommitGUI
) where

import qualified VCSGui.Common.Commit as C
import qualified VCSGui.Common.GtkHelper as H
import qualified VCSGui.Common.FilesInConflict as FiC
import qualified VCSGui.Common.MergeTool as M
import qualified VCSGui.Svn.Helper as SvnH

import VCSGui.Svn.AskPassword
import VCSGui.Common.ExceptionHandler

import qualified VCSWrapper.Svn as Svn

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)

{-  |
    Shows a GUI showing status of subversion and possibilites to commit/cancel.
-}
showCommitGUI :: Either M.MergeTool M.MergeToolSetter -- ^ 'MergeTool' is used for any possible conflicts. If not present user will be asked to provide 'MergeTool' on conflicts. 'MergeToolSetter' will be called for response.
                 -> Either Handler (Maybe String) -- ^ Either 'Handler' for password request or password (nothing for no password)
                 -> Svn.Ctx()
showCommitGUI eMergeToolSetter eitherHandlerOrPw = do
    conflictingFiles <- SvnH.getConflictingFiles
    case conflictingFiles of
        [] -> commonCommit
        _ -> FiC.showFilesInConflictGUI
                        Nothing
                        (conflictingFiles)
                        (Svn.getFilesInConflict)
                        (\fileToResolve -> Svn.resolved [fileToResolve] Nothing [])
                        eMergeToolSetter
                        $ commonCommit
    where
        commonCommit = C.showCommitGUI setUpTreeView (okCallback eitherHandlerOrPw)


okCallback :: Either Handler (Maybe String) -- ^ either callback for password request or password (nothing for no password)
            -> String               -- ^ commit message
            -> [FilePath]           -- ^ selected files
            -> [C.Option]           -- ^ TODO options
            -> Svn.Ctx ()
okCallback eitherHandlerOrPw msg filesToCommit _ =  do
                                case eitherHandlerOrPw of
                                    Left handler -> do
                                                        showAskpassGUI (ownHandler handler)
                                                        return ()
                                    Right pw     -> doCommit msg filesToCommit pw

        where
            doCommit msg filesToCommit mbPw = do
                                            Svn.add filesToCommit mbPw []
                                            Svn.commit filesToCommit msg mbPw []
                                            return()
            ownHandler :: Handler
                           -> Handler
            ownHandler handler = \result -> do
                                                case result of
                                                    Nothing       -> handler result
                                                    Just (_,mbPw) -> do
                                                                    doCommit msg filesToCommit mbPw
                                                                    handler result



setUpTreeView :: TreeView -> Svn.Ctx (ListStore C.SCFile)
setUpTreeView listView = do
    -- get status
    repoStatus <- Svn.status

    --check files for conflicts

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
        on renderer cellToggled $ \(columnId::String) -> do
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
