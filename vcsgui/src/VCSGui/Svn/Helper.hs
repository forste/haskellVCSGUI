-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.Helper
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

module VCSGui.Svn.Helper (
    getConflictingFiles
) where

import qualified VCSWrapper.Svn as Svn

getConflictingFiles :: Svn.Ctx [FilePath]
getConflictingFiles = do
    status <- Svn.status
    return [ Svn.filePath s | s <- status, (Svn.modification s) == Svn.Conflicting];
