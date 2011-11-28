-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Log
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Every function related to displaying the history of a Git repository is found in this module.
--
-----------------------------------------------------------------------------

module VCSGui.Git.Log (
    showLogGUI
) where

import qualified Graphics.UI.Gtk as Gtk
import qualified VCSGui.Common.Log as Common
import qualified VCSWrapper.Git as Git
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)


{- | Calls 'Common.showLogGUI' using Git. This will display all log entries. The branch to be displayed can be selected.
    Any commit can be checked out, creating a new branch if the commit is not already the HEAD of any branch.
    -}
showLogGUI :: Git.Ctx ()
showLogGUI = do
        log <- Git.simpleLog Nothing
        branches <- Git.localBranches
        Common.showLogGUI log [] (Just (branches, \branch -> Git.simpleLog (Just branch))) checkout
    where
    checkout log Nothing = Git.checkout (Just $ Git.commitID log) Nothing
    checkout log (Just selBranch) = do
        revBranch <- Git.revparse selBranch
        case ((Git.commitID log) == revBranch) of
            True -> do
                liftIO $ putStrLn "checking out selected Branch"
                Git.checkout (Just selBranch) Nothing
            False -> do
                liftIO $ putStrLn $ "checking out Commit " ++ (Git.commitID log) ++ ", asking for new branchname"
                branchname <- liftIO $ askForNewBranchname
                Git.checkout (Just $ Git.commitID log) (Just branchname)

    askForNewBranchname :: IO String
    askForNewBranchname = do
        dialog <- Gtk.dialogNew
        Gtk.dialogAddButton dialog "gtk-ok" Gtk.ResponseOk
        upper <- Gtk.dialogGetUpper dialog

        inputBranch <- Gtk.entryNew
        lblBranch <- Gtk.labelNew $ Just "Enter a new branchname:"
        box <- Gtk.hBoxNew False 2
        Gtk.containerAdd upper box
        Gtk.containerAdd box lblBranch
        Gtk.containerAdd box inputBranch

        Gtk.widgetShowAll dialog
        _ <- Gtk.dialogRun dialog
        branchname <- Gtk.entryGetText inputBranch
        Gtk.widgetDestroy dialog
        return branchname


















