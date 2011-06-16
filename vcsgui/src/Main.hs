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
import VCSGui.Common.Commit

-- test data
author = "test-author"
cwd = "/home/n0s/project1_work3"
testgladepath = "/home/n0s/home/leksahworkspace/svngui/gui/data/svn.glade"

main = do
    showGUI
        cwd
        author
        testgladepath
        (SVNGTKObjectAccessors
            "commit_dialog"
            "act_commit"
            "act_cancel"
            "buffer_commitmsg"
            "treeview_files"
            "bt_unlockTargets")
        SVN

