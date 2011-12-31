-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Mercurial.Log
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL Nothing
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Mercurial.Log (

) where

import qualified Graphics.UI.Gtk as Gtk
import qualified VCSGui.Common.Log as Common
import qualified VCSWrapper.Mercurial as MercMercurialurial
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)

{- | Calls 'Common.showLogGUI' using hg. This will display all log entries. The branch to be displayed can be selected.
    Any commit can be checked out, creating a new branch if the commit is not already the HEAD of any branch.
    -}
--showLogGUI :: Mercurial.Ctx ()
--showLogGUI = do
--        log <- Mercurial.simpleLog Nothing
--        branches <- Mercurial.localBranches
--        Common.showLogGUI log [] (Just (branches, \branch -> Mercurial.simpleLog (Just branch))) checkout
--    where
--    checkout log Nothing = Mercurial.checkout (Just $ Mercurial.commitID log) Nothing
--    checkout log (Just selBranch) = do
--        revBranch <- Mercurial.revparse selBranch
--        case ((Mercurial.commitID log) == revBranch) of
--            True -> do
--                liftIO $ putStrLn "checking out selected Branch"
--                Mercurial.checkout (Just selBranch) Nothing
--            False -> do
--                liftIO $ putStrLn $ "checking out Commit " ++ (Mercurial.commitID log) ++ ", asking for new branchname"
--                branchname <- liftIO $ askForNewBranchname
--                Mercurial.checkout (Just $ Mercurial.commitID log) (Just branchname)
--
--    askForNewBranchname :: IO String
--    askForNewBranchname = do
--        dialog <- Gtk.dialogNew
--        Gtk.dialogAddButton dialog "gtk-ok" Gtk.ResponseOk
--        upper <- Gtk.dialogGetUpper dialog
--
--        inputBranch <- Gtk.entryNew
--        lblBranch <- Gtk.labelNew $ Just "Enter a new branchname:"
--        box <- Gtk.hBoxNew False 2
--        Gtk.containerAdd upper box
--        Gtk.containerAdd box lblBranch
--        Gtk.containerAdd box inputBranch
--
--        Gtk.widgetShowAll dialog
--        _ <- Gtk.dialogRun dialog
--        branchname <- Gtk.entryGetText inputBranch
--        Gtk.widgetDestroy dialog
--        return branchname
