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
-- |
--
-----------------------------------------------------------------------------
module VCSGui.Commit (
    showGUI,
    GTKObjectAccessors(..),
    VCSType(..)
) where

import qualified VCSGui.Svn.Commit as Svn
import VCSGui.Types

-- loads gui objects and connects them
showGUI :: String               -- author
        -> FilePath             -- current working directory
        -> FilePath             -- glade
        -> GTKObjectAccessors   -- accessors for gtk objects
        -> VCSType              -- version control type
        -> IO()
showGUI cwd author gladepath gtkAccessors SVN = do
    Svn.showGUI cwd author gladepath gtkAccessors




