{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified VCSGui.Common.Log as Common
import qualified VCSWrapper.Git as Git
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import VCSGui.Common.Helpers (emptyTextToNothing)
import qualified Data.Text as T (unpack, pack)
import Data.Text (Text)
import qualified GI.Gtk.Objects.Dialog as Gtk
       (dialogRun, dialogGetContentArea, dialogAddButton, dialogNew)
import qualified GI.Gtk.Enums as Gtk (ResponseType(..), Orientation(..))
import qualified GI.Gtk.Objects.Entry as Gtk
       (entryGetText, entryNew)
import qualified GI.Gtk.Objects.Label as Gtk (labelNew)
import qualified GI.Gtk.Objects.Box as Gtk (boxNew)
import qualified GI.Gtk.Objects.Container as Gtk (containerAdd)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk.Objects.Box (Box(..))
import qualified GI.Gtk.Objects.Widget as Gtk
       (widgetDestroy, widgetShowAll)


{- | Calls 'Common.showLogGUI' using Git. This will display all log entries. The branch to be displayed can be selected.
    Any commit can be checked out, creating a new branch if the commit is not already the HEAD of any branch.
    -}
showLogGUI :: Git.Ctx ()
showLogGUI = do
        log <- Git.simpleLog Nothing
        branches <- Git.localBranches
        Common.showLogGUI log [] (Just (branches, \branch -> Git.simpleLog (Just branch))) checkout False
    where
    checkout log Nothing = Git.checkout (Just $ Git.commitID log) Nothing
    checkout log (Just selBranch) = do
        revBranch <- Git.revparse selBranch
        case ((Git.commitID log) == revBranch) of
            True -> do
                liftIO $ putStrLn "checking out selected Branch"
                Git.checkout (Just selBranch) Nothing
            False -> do
                liftIO $ putStrLn $ "checking out Commit " ++ (T.unpack $ Git.commitID log) ++ ", asking for new branchname"
                mbBranchname <- liftIO $ askForNewBranchname
                Git.checkout (Just $ Git.commitID log) (mbBranchname)

    askForNewBranchname :: IO (Maybe Text)
    askForNewBranchname = do
        dialog <- Gtk.dialogNew
        Gtk.dialogAddButton dialog ("gtk-ok"::Text) (fromIntegral $ fromEnum Gtk.ResponseTypeOk)
        upper <- Gtk.dialogGetContentArea dialog >>= unsafeCastTo Box

        inputBranch <- Gtk.entryNew
        lblBranch <- Gtk.labelNew $ Just ("Enter a new branchname (empty for anonym branch):" :: Text)
        box <- Gtk.boxNew Gtk.OrientationHorizontal 2
        Gtk.containerAdd upper box
        Gtk.containerAdd box lblBranch
        Gtk.containerAdd box inputBranch

        Gtk.widgetShowAll dialog
        _ <- Gtk.dialogRun dialog
        branchname <- Gtk.entryGetText inputBranch
        Gtk.widgetDestroy dialog
        return $ emptyTextToNothing branchname


















