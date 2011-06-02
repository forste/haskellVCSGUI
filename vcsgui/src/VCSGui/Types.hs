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
    GTKObjectAccessors (..),
    VCSType (..)
) where

import Common.Types
import qualified Lib.Svn as Svn

data VCSType = SVN | GIT

data GTKObjectAccessors = GTKObjectAccessors
    {
    gtkCommitDialog :: String,
    gtkActCommit :: String,
    gtkActCancel :: String,
    gtkBufferCommitMsg :: String,
    gtkListView :: String
    }

