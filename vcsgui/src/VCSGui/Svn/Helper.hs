-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.Helper
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

module VCSGui.Svn.Helper (
    getConflictingFiles
) where

import qualified VCSWrapper.Svn as Svn

getConflictingFiles :: Svn.Ctx [FilePath]
getConflictingFiles = do
    status <- Svn.status
    return [ Svn.filePath s | s <- status, (Svn.modification s) == Svn.Conflicting];
