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
    openLogWindow
) where

import VCSGui.Common.Log
import qualified VCSWrapper.Git as Git

openLogWindow :: Git.Ctx ()
openLogWindow = do
        log <- Git.simpleLog
        showLogGUI log [] Nothing checkout
    where
    checkout log _ = Git.checkout (Just $ Git.commitID log) Nothing

