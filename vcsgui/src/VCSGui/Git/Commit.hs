{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Commit
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Every function related to commiting changes to the Git repository is found in this module.
--
-----------------------------------------------------------------------------

module VCSGui.Git.Commit (
    showCommitGUI
) where

import Control.Monad.Trans(liftIO)
import Control.Monad.Reader(ask)

import VCSGui.Common.GtkHelper
import qualified VCSGui.Common.Commit as Commit

import qualified VCSWrapper.Git as Git
import qualified VCSWrapper.Common as Wrapper
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import GI.Gtk.Objects.TreeView (treeViewSetModel, TreeView(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreSetValue, seqStoreGetValue, seqStoreIterToIndex,
        seqStoreNew, SeqStore(..))
import GI.Gtk.Objects.CellRendererToggle
       (onCellRendererToggleToggled, cellRendererToggleNew)
import GI.Gtk.Interfaces.TreeModel (treeModelGetIterFromString)
import GI.Gtk
       (setCellRendererTextText, setCellRendererToggleActive)

doCommit :: Text -> [FilePath] -> [Commit.Option] -> Wrapper.Ctx ()
doCommit commitMsg files _ = do
    Git.add files
    (Wrapper.Config _ _ mbAuthor _) <- ask
    case mbAuthor of
        Nothing -> Git.commit files Nothing commitMsg []
        Just (Wrapper.Author author mbEmail) -> do
            case mbEmail of
                Nothing -> Git.commit files (Just (author, "noEmailSet@noEmailSet")) commitMsg []
                Just m -> Git.commit files (Just (author, m)) commitMsg []

{- | Calls 'Commit.showCommitGUI' with a 'Data.GI.Gtk.SeqStore' and 'OkCallBack' setup for Git.
    This will display a window to enter a commit message and select the files to be commited by this commit.
-}
showCommitGUI :: Git.Ctx ()
showCommitGUI = do
    Commit.showCommitGUI setupSeqStore doCommit

setupSeqStore :: TreeView -> Wrapper.Ctx (SeqStore Commit.SCFile)
setupSeqStore view = do
        repoStatus <- Git.status
        --GITSCFile Bool FilePath Text
        let selectedF = [Commit.GITSCFile True fp (T.pack $ show mod) | (Wrapper.GITStatus fp mod) <- repoStatus, mod == Wrapper.Modified || mod == Wrapper.Added]
            notSelectedF = [Commit.GITSCFile False fp (T.pack $ show mod) | (Wrapper.GITStatus fp mod) <- repoStatus, mod /= Wrapper.Modified && mod /= Wrapper.Added]

        liftIO $ do
            store <- seqStoreNew (selectedF ++ notSelectedF)
            treeViewSetModel view $ Just store
            let item = (store, view)

            toggleRenderer <- cellRendererToggleNew
            addColumnToTreeView' item toggleRenderer "Commit" (\cell (Commit.GITSCFile s _ _) -> setCellRendererToggleActive cell s)
            addTextColumnToTreeView' item "File" (\cell (Commit.GITSCFile _ p _) -> setCellRendererTextText cell $ T.pack p)
            addTextColumnToTreeView' item "File status" (\cell (Commit.GITSCFile _ _ m) -> setCellRendererTextText cell m)

            -- register toggle renderer
            onCellRendererToggleToggled toggleRenderer $ \filepath -> do
                putStrLn ("toggle called: " ++ T.unpack filepath)

                treeModelGetIterFromString store filepath >>= \case
                    (True, treeIter) -> do
                        n <- seqStoreIterToIndex treeIter
                        value <- seqStoreGetValue store n
                        let newValue = (\(Commit.GITSCFile b fp m) -> Commit.GITSCFile (not b) fp m) value
                        seqStoreSetValue store n newValue
                    _ -> return ()
            return store

