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
        log <- Git.simpleLog
        Common.showLogGUI log [] (Just (["master", "otherBranch"], \_ -> return [])) checkout
    where
    checkout log _ = Git.checkout (Just $ Git.commitID log) Nothing

