-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Types
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
{-# LANGUAGE RankNTypes #-}
module VCSGui.Types (
    VCSType (..),
    Config,
    GTKObjects (..),
    module Common.Types
) where

import Common.Types
import qualified Lib.Svn as Svn

data VCSType = VCSType
    {
    doAdd :: forall t. (Ctx t -> IO t) -> String -> IO(),
    doCommit :: forall t. (Ctx t -> IO t) -> String -> [FilePath] -> IO(),
    getStatus :: forall t. (Ctx t -> IO t) -> IO [(String, Modification)],
    runWithConfig :: forall t. Ctx t -> IO t
     }

data GTKObjects = GTKObjects
    {
    gtkCommitDialog :: String,
    gtkActCommit :: String,
    gtkActCancel :: String,
    gtkBufferCommitMsg :: String,
    gtkListView :: String
    }

doAdd :: forall t. (Ctx t -> IO t) -- executor
        -> String -- file to add
        -> IO()
svnDoAdd executor file = executor $ Svn.add [file] []

svnDoCommit :: forall t. (Ctx t -> IO t) -- executor
            -> String -- msg
            -> [FilePath] -- files
            -> IO()
svnDoCommit executor msg files = executor $ Svn.commit files author msg []

author = "testauthor"

svnGetStatus :: forall t. (Ctx t -> IO t) -> IO [(String, Modification)]
svnGetStatus executor = executor $ Svn.status []

svnMakeConfig :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Config
svnMakeConfig = Svn.makeConfig
