-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Log
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

module VCSGui.Git.Log (
    showLogGUI
) where

import qualified VCSGui.Common.Log as Common
import qualified VCSWrapper.Git as Git

showLogGUI :: Git.Ctx ()
showLogGUI = do
        log <- Git.simpleLog Nothing
        branches <- Git.localBranches
        Common.showLogGUI log [] (Just (branches, \branch -> Git.simpleLog (Just branch))) checkout
    where
    checkout log _ = Git.checkout (Just $ Git.commitID log) Nothing

