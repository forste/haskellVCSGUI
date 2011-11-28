-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Types
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
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

