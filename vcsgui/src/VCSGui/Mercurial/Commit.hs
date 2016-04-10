{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Mercurial.Commit
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL Nothing
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Mercurial.Commit (
    showCommitGUI
) where

import Control.Monad.Trans(liftIO)
import Control.Monad.Reader(ask)

import VCSGui.Common.GtkHelper
import qualified VCSGui.Common.Commit as Commit

import qualified VCSWrapper.Mercurial as Mercurial
import qualified VCSWrapper.Common as Wrapper
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import GI.Gtk.Objects.TreeView (treeViewSetModel, TreeView(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreSetValue, seqStoreGetValue, seqStoreIterToIndex,
        seqStoreNew, SeqStore(..))
import GI.Gtk.Objects.CellRendererToggle
       (onCellRendererToggleToggled, cellRendererToggleNew)
import Data.GI.Base.Attributes (AttrLabelProxy(..), AttrOp(..))
import GI.Gtk.Interfaces.TreeModel (treeModelGetIterFromString)

_text = AttrLabelProxy :: AttrLabelProxy "text"
_active = AttrLabelProxy :: AttrLabelProxy "active"

doCommit :: Text -> [FilePath] -> [Commit.Option] -> Wrapper.Ctx ()
doCommit commitMsg files _ = do
    Mercurial.addremove files
    Mercurial.commit files commitMsg []

{- |
    Calls 'Commit.showCommitGUI' with a 'Data.GI.Gtk.SeqStore' and 'OkCallBack' setup for Mercurial.
    This will display a window to enter a commit message and select the files to be commited by this commit.
-}
showCommitGUI :: Mercurial.Ctx ()
showCommitGUI = do
    Commit.showCommitGUI setupSeqStore doCommit

--TODO this is copy&pasted from git implementation refactor it and use abstract version
setupSeqStore :: TreeView -> Wrapper.Ctx (SeqStore Commit.SCFile)
setupSeqStore view = do
        repoStatus <- Mercurial.status
        --GITSCFile Bool FilePath Text
        let selectedF = [Commit.GITSCFile True fp (T.pack $ show mod) | (Wrapper.GITStatus fp mod) <- repoStatus, mod == Wrapper.Modified || mod == Wrapper.Added]
            notSelectedF = [Commit.GITSCFile False fp (T.pack $ show mod) | (Wrapper.GITStatus fp mod) <- repoStatus, mod /= Wrapper.Modified && mod /= Wrapper.Added]

        liftIO $ do
            store <- seqStoreNew (selectedF ++ notSelectedF)
            treeViewSetModel view (Just store)
            let item = (store, view)

            toggleRenderer <- cellRendererToggleNew
            addColumnToTreeView' item toggleRenderer "Commit" (\(Commit.GITSCFile s _ _)-> [_active := s])
            addTextColumnToTreeView' item "File" (\(Commit.GITSCFile _ p _) -> [_text := T.pack p])
            addTextColumnToTreeView' item "File status" (\(Commit.GITSCFile _ _ m) -> [_text := m])

            -- register toggle renderer
            onCellRendererToggleToggled toggleRenderer $ \filepath -> do
                putStrLn ("toggle called: " ++ T.unpack filepath)

                treeModelGetIterFromString store filepath >>= \case
                    (True, treeIter) -> do
                        n <- seqStoreIterToIndex treeIter
                        value <- seqStoreGetValue store n
                        let newValue = (\(Commit.GITSCFile b fp m) -> Commit.GITSCFile (not b) fp m) value
                        seqStoreSetValue store n newValue
                        return ()
                    _ -> return ()
            return store
