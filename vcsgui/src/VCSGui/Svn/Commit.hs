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
-- | TODO this module needs refactoring. setUpTreeView should use GtkHelper to avoid code redundance
--
-----------------------------------------------------------------------------
module VCSGui.Svn.Commit (
    showCommitGUI
) where

import qualified VCSGui.Common.Commit as C
import VCSGui.Common.Types
import Graphics.UI.Gtk
import qualified VCSWrapper.Svn as Svn
import Control.Monad.Trans(liftIO)
import VCSWrapper.Common

showCommitGUI :: Svn.Ctx()
showCommitGUI = C.showCommitGUI setUpTreeView okCallback



okCallback :: String    -- ^ commit message
            -> [FilePath] -- ^ selected files
            -> [C.Option] -- ^ TODO options
            -> Config
            -> IO()
okCallback msg filesToCommit _ config =  do
                                putStrLn $ "Pressed ok"
                                putStrLn $ "Files to commit: "++show filesToCommit
                                putStrLn $ "Commit message: "++msg
                                runWithConfig $ Svn.add [] filesToCommit
                                runWithConfig $ Svn.commit filesToCommit "toBeReplacedInConfig" msg []
                                return ()
                                where
                                    runWithConfig = Svn.runVcs config


setUpTreeView :: TreeView -> Svn.Ctx (ListStore C.SCFile)
setUpTreeView listView = do
    -- get status
    repoStatus <- Svn.status []

    -- create model
    listStore <- liftIO $ listStoreNew [
            (C.SVNSCFile (ctxSelect (Svn.modification status))
                         (Svn.filePath status)
                         (show (Svn.modification status))
                         (Svn.isLocked status))
            | status <- repoStatus]
    liftIO $ treeViewSetModel listView listStore

    -- selection column
    selectedPathColumn <- liftIO $ treeViewColumnNew
    liftIO $ set selectedPathColumn [treeViewColumnTitle := "Files to commit" ]
    liftIO $ treeViewAppendColumn listView selectedPathColumn

    -- render selection
    selectedRenderer <- liftIO $ cellRendererToggleNew
    liftIO $ treeViewColumnPackStart selectedPathColumn selectedRenderer False
    liftIO $ cellLayoutSetAttributes selectedPathColumn selectedRenderer listStore $
        \scf -> [cellToggleActive := C.selected scf]

    -- connect select action
    liftIO $ on selectedRenderer cellToggled $ \columnId -> do
                            Just treeIter <- treeModelGetIterFromString listStore columnId
                            value <- listStoreGetValue listStore $ listStoreIterToIndex treeIter
                            let newValue = (\(C.SVNSCFile bool fp s l) -> C.SVNSCFile (not bool) fp s l)
                                            value
                            listStoreSetValue listStore (listStoreIterToIndex treeIter) newValue
                            return ()
    -- render path
    pathRenderer <- liftIO $ cellRendererTextNew
    liftIO $ treeViewColumnPackEnd selectedPathColumn pathRenderer True
    liftIO $ cellLayoutSetAttributes selectedPathColumn pathRenderer listStore $
        \scf -> [cellText := C.filePath scf]

    -- status column
    statusColumn <- liftIO $ treeViewColumnNew
    liftIO $ set statusColumn [treeViewColumnTitle := "Status"]
    liftIO $ treeViewAppendColumn listView statusColumn

    -- render status
    statusRenderer <- liftIO $ cellRendererTextNew
    liftIO $ treeViewColumnPackEnd statusColumn statusRenderer False
    liftIO $ cellLayoutSetAttributes statusColumn statusRenderer listStore $
        \scf -> [cellText := C.status scf]

    -- lock column
    lockColumn <- liftIO $ treeViewColumnNew
    liftIO $ set lockColumn [treeViewColumnTitle := "Locked" ]
    liftIO $ treeViewAppendColumn listView lockColumn

    -- render lock
    lockRenderer <- liftIO $ cellRendererToggleNew
    liftIO $ treeViewColumnPackEnd lockColumn lockRenderer False
    liftIO $ cellLayoutSetAttributes lockColumn lockRenderer listStore $
        \scf  -> [cellToggleActive := C.isLocked scf]
    return listStore
    where
        ctxSelect status =  status == Svn.Added || status == Svn.Deleted || status==Svn.Modified ||
                            status == Svn.Replaced
