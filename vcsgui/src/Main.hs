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
import qualified VCSGui.Svn as Svn
import VCSWrapper.Common

import qualified VCSGui.Git.Log as GitLog
import qualified VCSGui.Git.Commit as GitCommit
import Graphics.UI.Gtk

--
--svn
--

--
-- commit
--

--{-
-- test data
author = "test-author"
cwd = "/home/n0s/project1_work3"
testgladepath = "/home/n0s/home/leksahworkspace/svngui/gui/data/svn.glade"

{-
main = do
    initGUI
    runWithConfig $ Svn.showCommitGUI
    mainGUI
    mainQuit
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
--log
--

{-
cwdGit = "/home/n0s/project1_work3"

main = do
        initGUI
        runWithConfig $
            Svn.showLogGUI
        mainGUI
        mainQuit
    where
        runWithConfig = runVcs $ makeConfig (Just cwdGit) Nothing Nothing
---}

--
--git
--

--
--log
--

cwdGit = "/home/n0s-ubuntu/testrepo"

{-
main = do
        initGUI
        runWithConfig $
            GitLog.showLogGUI
        mainGUI
    where
        runWithConfig = runVcs $ makeConfig (Just cwdGit) Nothing Nothing
-}


--
-- commit
--
--{-
main = do
    initGUI
    runWithConfig $
        GitCommit.showCommitGUI
    mainGUI
    where
        runWithConfig = runVcs $ makeConfig (Just cwdGit) Nothing Nothing

---}






