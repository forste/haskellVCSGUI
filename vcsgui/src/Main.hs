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

module Main (
    main
) where

import VCSGui.Common.Types
import VCSGui.Svn.Commit
import VCSWrapper.Common

-- test data
author = "test-author"
cwd = "/home/n0s/project1_work3"
testgladepath = "/home/n0s/home/leksahworkspace/svngui/gui/data/svn.glade"

main = do
    runWithConfig $ showGUI
        Main.author
        testgladepath
        (SVNGTKObjectAccessors
            "commit_dialog"
            "act_commit"
            "act_cancel"
            "buffer_commitmsg"
            "treeview_files"
            "bt_unlockTargets")
    where
        runWithConfig = runVcs $ makeConfig (Just cwd) Nothing Nothing
