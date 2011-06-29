-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.Log
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

module VCSGui.Svn.Log (
    showLogGUI
) where

import qualified VCSGui.Common.Log as C
import qualified VCSWrapper.Svn as Svn

showLogGUI :: Svn.Ctx ()
showLogGUI = do
        logEntries <- Svn.simpleLog
        C.showLogGUI logEntries [] Nothing $ checkout
    where
    checkout logEntry _ = Svn.revert (revision logEntry) Nothing [] -- TODO password handler
    revision logEntry = read $ Svn.commitID logEntry :: Integer
