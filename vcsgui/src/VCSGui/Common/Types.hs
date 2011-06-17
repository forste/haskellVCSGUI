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
module VCSGui.Common.Types (
    GTKObjectAccessors (..),
    VCSType (..)
) where

data VCSType = SVN | GIT

data GTKObjectAccessors = SVNGTKObjectAccessors
    {
    gtkCommitDialog :: String
    ,gtkActCommit :: String
    ,gtkActCancel :: String
    ,gtkBufferCommitMsg :: String
    ,gtkListView :: String
    ,gtkBtUnlockTargets :: String
    }
