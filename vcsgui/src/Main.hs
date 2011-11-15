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
import VCSGui.Common.MergeTool
import qualified VCSGui.Svn as Svn
import qualified VCSWrapper.Common as Wrapper

import qualified VCSGui.Git.Log as GitLog
import qualified VCSGui.Git.Commit as GitCommit
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
--
--svn
--

--
-- commit
--

--{-
cwd = "/home/forste/svnreps/project1_work1"
main = do
    initGUI
    runWithConfig $ Svn.showCommitGUI (Left handler) $ Right (\tool -> putStrLn ("Setter called with " ++ fullPath tool))
    mainGUI
    mainQuit
    where
        runWithConfig = Wrapper.runVcs $ Wrapper.makeConfig (Just cwd) Nothing Nothing
        handler = (\result -> liftIO $ putStrLn $ show result)
---}

--
-- checkout
--

{-
cwd = "/home/n0s/"
main = do
    initGUI
    runWithConfig $ Svn.showCheckoutGUI
    mainGUI
    mainQuit
    where
        runWithConfig = runVcs $ makeConfig (Just cwd) Nothing Nothing
-}

--
--log
--

{-
cwd = "/home/forste/project1_work"
main = do
        initGUI
        runWithConfig $
            Svn.showLogGUI
        mainGUI
        mainQuit
    where
        runWithConfig = Wrapper.runVcs $ Wrapper.makeConfig (Just cwd) Nothing Nothing
-}

--
--askpass
--
--main = do
--        initGUI
--        runWithConfig $ Svn.showAskpassGUI (\result -> liftIO $ putStrLn $ "Result "++ show result)
--        mainGUI
--        mainQuit
--      where
--        runWithConfig = Wrapper.runVcs $ Wrapper.makeConfig Nothing Nothing Nothing

--
--git
--

--
--log
--

cwdGit = "/home/forste/leksahWorkspace/leksah"
--cwdGit = "/home/n0s-ubuntu/testrepo"
{-
main = do
        initGUI
        runWithConfig $
            GitLog.showLogGUI
        mainGUI
    where
        runWithConfig = Wrapper.runVcs $ Wrapper.makeConfig (Just cwdGit) Nothing Nothing
-}


--
-- commit
--

{-
main = do
    initGUI
    runWithConfig $
        GitCommit.showCommitGUI
    mainGUI
    where
        runWithConfig = Wrapper.runVcs $ Wrapper.makeConfig (Just cwdGit) Nothing Nothing
-}


--
-- common
--

-- setup repo
{-
cwd = "/home/forste/leksahWorkspace/leksah"
main = do
    initGUI
    Svn.showSetupConfigGUI config $ \(Just(x,y)) -> putStrLn $ "Config:" ++ show y ++ ", Chosen vcs:" ++ show x
    mainGUI
    where
        config = Just (Wrapper.GIT, Wrapper.makeConfig Nothing Nothing Nothing)
--        config = Just (Wrapper.GIT, Wrapper.makeConfig (Just cwd) Nothing Nothing)
-}
