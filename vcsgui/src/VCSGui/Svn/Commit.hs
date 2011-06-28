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
module VCSGui.Svn.Commit (
    showCommitGUI
    ,Svn.Config
    ,Svn.Ctx
) where

import qualified VCSGui.Common.Commit as C
import qualified VCSGui.Common.GtkHelper as H

import VCSGui.Svn.AskPassword

import qualified VCSWrapper.Svn as Svn

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)

{-  | Displays a window, showing status of subversion and actions to commit/cancel.
    | Give callback will be called on success with chosen password and boolean whether password should be saved for session.
-}
showCommitGUI :: Maybe (Maybe (Maybe (Bool, String))  -- ^ args passed to handler, Nothing if operation is aborted (cancel,quit)
                                                -- ^ else Just Nothing if use password = false, else Just (saveForSession, pw)
                    -> Svn.Ctx ())    -- ^ callback for password request
                 -> Svn.Ctx()
showCommitGUI passwordHandler = C.showCommitGUI setUpTreeView (okCallback passwordHandler)

okCallback :: Maybe (Maybe (Maybe (Bool, String))

                  -> Svn.Ctx ())    -- ^ password handler
            -> String               -- ^ commit message
            -> [FilePath]           -- ^ selected files
            -> [C.Option]           -- ^ TODO options
            -> Svn.Ctx ()
okCallback Nothing  msg filesToCommit _ = do
                                doCommit msg filesToCommit
okCallback (Just passwordHandler) msg filesToCommit _ =  do
                                showAskpassGUI (ownHandler passwordHandler)
                                return ()
        where
            ownHandler :: (Maybe (Maybe (Bool, String)) -> Svn.Ctx ())
                           -> (Maybe (Maybe (Bool, String)) -> Svn.Ctx ())
            ownHandler handler = \result -> do
                                                case result of
                                                    Nothing -> handler result
                                                    _       -> do
                                                                doCommit msg filesToCommit
                                                                handler result
doCommit msg filesToCommit = do
    Svn.add [] filesToCommit
    Svn.commit filesToCommit msg []
    return()


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
