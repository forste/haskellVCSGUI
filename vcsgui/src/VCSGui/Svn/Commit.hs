{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
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

import Control.Monad.Trans(liftIO)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import GI.Gtk.Objects.TreeView (treeViewSetModel, TreeView(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreSetValue, seqStoreIterToIndex, seqStoreGetValue,
        seqStoreNew, SeqStore(..))
import GI.Gtk.Objects.CellRendererToggle
       (onCellRendererToggleToggled, cellRendererToggleNew)
import GI.Gtk.Interfaces.TreeModel (treeModelGetIterFromString)
import GI.Gtk.Objects.CellRendererText (cellRendererTextNew)
import GI.Gtk
       (setCellRendererTextText, setCellRendererToggleActive)

{-  |
    Shows a GUI showing status of subversion and possibilites to commit/cancel.
-}
showCommitGUI :: Either M.MergeTool M.MergeToolSetter -- ^ 'MergeTool' is used for any possible conflicts. If not present user will be asked to provide 'MergeTool' on conflicts. 'MergeToolSetter' will be called for response.
                 -> Either Handler (Maybe Text) -- ^ Either 'Handler' for password request or password (nothing for no password)
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


okCallback :: Either Handler (Maybe Text) -- ^ either callback for password request or password (nothing for no password)
            -> Text               -- ^ commit message
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



setUpTreeView :: TreeView -> Svn.Ctx (SeqStore C.SCFile)
setUpTreeView listView = do
    -- get status
    repoStatus <- Svn.status

    --check files for conflicts

    liftIO $ do
        -- create model
        seqStore <- seqStoreNew [
                (C.SVNSCFile (ctxSelect (Svn.modification status))
                             (Svn.filePath status)
                             (T.pack . show $ Svn.modification status)
                             (Svn.isLocked status))
                | status <- repoStatus]
        treeViewSetModel listView (Just seqStore)

        let treeViewItem = (seqStore, listView)

        renderer <- cellRendererToggleNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               ""
                               $ \cell scf -> setCellRendererToggleActive cell $ C.selected scf

        -- connect select action
        onCellRendererToggleToggled renderer $ \(columnId :: Text) -> do
                                (True, treeIter) <- treeModelGetIterFromString seqStore columnId
                                n <- seqStoreIterToIndex treeIter
                                value <- seqStoreGetValue seqStore n
                                let newValue = (\(C.SVNSCFile bool fp s l) -> C.SVNSCFile (not bool) fp s l)
                                                value
                                seqStoreSetValue seqStore n newValue
                                return ()

        renderer <- cellRendererTextNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Files to commit"
                               $ \cell scf -> setCellRendererTextText cell . T.pack $ C.filePath scf

        renderer <- cellRendererTextNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Status"
                               $ \cell scf -> setCellRendererTextText cell $ C.status scf

        renderer <- cellRendererToggleNew
        H.addColumnToTreeView' treeViewItem
                               renderer
                               "Locked"
                               $ \cell scf -> setCellRendererToggleActive cell $ C.isLocked scf
        return seqStore
    where
        ctxSelect status =  status == Svn.Added || status == Svn.Deleted || status==Svn.Modified ||
                            status == Svn.Replaced
