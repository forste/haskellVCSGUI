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
import VCSGui.Svn.Checkout
import VCSWrapper.Common

import qualified VCSGui.Git.Log as GitLog
import Graphics.UI.Gtk

--
--svn
--

--
-- commit
--

 {-
 test data
author = "test-author"
cwd = "/home/n0s/project1_work3"
testgladepath = "/home/n0s/home/leksahworkspace/svngui/gui/data/svn.glade"


main = do
    runWithConfig $ VCSGui.Svn.Commit.showGUI
        Main.author
        testgladepath
        (SVNGTKObjectAccessors
            "commit_dialog"
            "act_commit"
            "act_cancel"
            "buffer_commitmsg"
            "treeview_files"
            "bt_unlockTargets")
    runWithConfig $ VCSGui.Svn.Checkout.showGUI
    where
        runWithConfig = runVcs $ makeConfig (Just cwd) Nothing Nothing
-}

--
-- checkout
--

{-
cwd = "/home/n0s/"
testgladepath = "/home/n0s/home/leksahworkspace/svngui/gui/data/svn_checkout2.glade"
main = do
    runWithConfig $ VCSGui.Svn.Checkout.showGUI
        testgladepath
        (SVNCheckoutObjectAccessors
            "window_checkout"
            "act_checkout"
            "act_cancel"
            "buffer_url"
            "buffer_revision"
            "buffer_path")
    where
        runWithConfig = runVcs $ makeConfig (Just cwd) Nothing Nothing
---}

--
--git
--

--
--log
--

--{-
cwdGit = "/home/n0s-ubuntu/testrepo"

main = do
        initGUI
        runWithConfig $
            GitLog.showLogGUI
        mainGUI
        mainQuit
    where
        runWithConfig = runVcs $ makeConfig (Just cwdGit) Nothing Nothing
---}









