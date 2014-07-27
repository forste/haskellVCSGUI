{-# LANGUAGE OverloadedStrings #-}
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

import Graphics.UI.Gtk

import VCSGui.Common.GtkHelper
import qualified VCSGui.Common.Commit as Commit

import qualified VCSWrapper.Git as Git
import qualified VCSWrapper.Common as Wrapper
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)


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

{- | Calls 'Commit.showCommitGUI' with a 'Graphics.UI.Gtk.ListStore' and 'OkCallBack' setup for Git.
    This will display a window to enter a commit message and select the files to be commited by this commit.
-}
showCommitGUI :: Git.Ctx ()
showCommitGUI = do
    Commit.showCommitGUI setupListStore doCommit

setupListStore :: TreeView -> Wrapper.Ctx (ListStore Commit.SCFile)
setupListStore view = do
        repoStatus <- Git.status
        --GITSCFile Bool FilePath Text
        let selectedF = [Commit.GITSCFile True fp (T.pack $ show mod) | (Wrapper.GITStatus fp mod) <- repoStatus, mod == Wrapper.Modified || mod == Wrapper.Added]
            notSelectedF = [Commit.GITSCFile False fp (T.pack $ show mod) | (Wrapper.GITStatus fp mod) <- repoStatus, mod /= Wrapper.Modified && mod /= Wrapper.Added]

        liftIO $ do
            store <- listStoreNew (selectedF ++ notSelectedF)
            treeViewSetModel view store
            let item = (store, view)

            toggleRenderer <- cellRendererToggleNew
            addColumnToTreeView' item toggleRenderer "Commit" (\(Commit.GITSCFile s _ _)-> [cellToggleActive := s])
            addTextColumnToTreeView' item "File" (\(Commit.GITSCFile _ p _) -> [cellText := T.pack p])
            addTextColumnToTreeView' item "File status" (\(Commit.GITSCFile _ _ m) -> [cellText := m])

            -- register toggle renderer
            on toggleRenderer cellToggled $ \filepath -> do
                putStrLn ("toggle called: " ++ T.unpack filepath)

                Just treeIter <- treeModelGetIterFromString store filepath
                value <- listStoreGetValue store $ listStoreIterToIndex treeIter
                let newValue = (\(Commit.GITSCFile b fp m) -> Commit.GITSCFile (not b) fp m) value
                listStoreSetValue store (listStoreIterToIndex treeIter) newValue
                return ()
            return store

