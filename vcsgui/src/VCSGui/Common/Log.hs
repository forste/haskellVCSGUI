-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.Log
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Functions to show a log window. This mostly hides the window-building tasks from the specific VCS implementation.
--
-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VCSGui.Common.Log (
    showLogGUI
) where

import Control.Monad.Reader
import qualified Graphics.UI.Gtk as Gtk
import Data.Maybe

import VCSGui.Common.GtkHelper
import qualified VCSWrapper.Common as Common


import Data.Maybe (fromMaybe)
import Paths_vcsgui(getDataFileName)

getGladepath = getDataFileName "data/guiCommonLog.glade"

data LogConfig a = LogConfig {
    options :: [String]
    ,treeViewSetter :: Gtk.TreeView -> TreeViewItem a
}


--------------------------------------------------
---- LOG VIEWER / CHECKOUT
--------------------------------------------------

data LogGUI = LogGUI {
    logWin :: WindowItem
    , logTreeView :: TreeViewItem Common.LogEntry
    , lblRevisionDetails :: LabelItem
    , actCheckout :: ActionItem
    , actLogCancel :: ActionItem
    , comboBranch :: ComboBoxItem
    , lblBranch :: LabelItem
}

-- | Show the history of a repository.
showLogGUI :: [Common.LogEntry]
            -- ^ logEntries to be displayed initially
            -> [String]
            -- ^ options will be displayed in a menu as checkboxes (TODO this is currently not implemented)
            -> Maybe ((String, [String]), (String -> Common.Ctx [Common.LogEntry]))
            -- ^ (list of branchnames to display, Function called when a different branch is selected)
            --
            -- The function will be called with the selected branchname to repopulate the displayed LogEntries.
            -- If 'Nothing', no branch selection will be displayed.
            -> (Common.LogEntry -> Maybe String -> Common.Ctx ())
            -- ^ (selected line, name of the branch to checkout from)
            --
            -- This function is called on checkout action. The window will be closed afterwards.
            -> Bool -- ^ Add column to display branch name
            -> Common.Ctx ()
showLogGUI logEntries options Nothing doCheckoutFn displayBranchNames = guiWithoutBranches logEntries options doCheckoutFn displayBranchNames >> return ()
showLogGUI logEntries options (Just (branches, changeBranchFn)) doCheckoutFn displayBranchNames = do
        gui <- guiWithoutBranches logEntries options doCheckoutFn displayBranchNames
        guiAddBranches gui branches changeBranchFn
        return ()

guiWithoutBranches :: [Common.LogEntry]
                    -> [String]
                    -> (Common.LogEntry
                        -> (Maybe String)
                        -> Common.Ctx ())
                    -> Bool -- ^ Add column to display branch name
                    -> Common.Ctx LogGUI
guiWithoutBranches logEntries options doCheckoutFn displayBranchNames = do
        gui <- loadLogGui logEntries
        liftIO $ setupLogColumns gui displayBranchNames

        -- connect gui elements
        liftIO $ registerClose $ logWin gui
        liftIO $ registerCloseAction (actLogCancel gui) (logWin gui)

        config <- ask
        liftIO $ Gtk.on (getItem (actCheckout gui)) Gtk.actionActivated $
            doCheckout' config (logTreeView gui) (comboBranch gui)
                >> (closeWin (logWin gui))

        -- show window
        liftIO $ Gtk.widgetShowAll $ getItem $ logWin gui
        return gui
    where
    doCheckout' :: Common.Config -> TreeViewItem Common.LogEntry -> ComboBoxItem -> IO ()
    doCheckout' cfg (_, (store, view), _) combo = do
        (path, _) <- Gtk.treeViewGetCursor view -- TODO fix nothing selected bug here
        Just treeIter <- Gtk.treeModelGetIter store path
        selectedLog <- Gtk.listStoreGetValue store $ Gtk.listStoreIterToIndex treeIter
        selectedBranch <- get combo
        Common.runVcs cfg $ doCheckoutFn selectedLog selectedBranch

    setupLogColumns :: LogGUI -> Bool -> IO ()
    setupLogColumns gui displayBranchNames = do
        let item = (logTreeView gui)
        addTextColumnToTreeView item "Subject" (\Common.LogEntry { Common.subject = t } -> [Gtk.cellText Gtk.:= t])
        addTextColumnToTreeView item "Author" (\Common.LogEntry { Common.author = t, Common.email = mail } -> [Gtk.cellText Gtk.:= (t ++ " <" ++ mail ++ ">")])
        addTextColumnToTreeView item "Date" (\Common.LogEntry { Common.date = t } -> [Gtk.cellText Gtk.:= t])
        case displayBranchNames of
            True -> addTextColumnToTreeView item "Branch" (\Common.LogEntry { Common.mbBranch = t } -> [Gtk.cellText Gtk.:= (fromMaybe "" t)])
            False -> return()
        return ()

guiAddBranches :: LogGUI -> (String, [String]) -> (String -> Common.Ctx [Common.LogEntry]) -> Common.Ctx ()
guiAddBranches gui (curBranch, otherBranches) changeBranchFn = do
        -- set branch selection visible
        liftIO $ Gtk.set (getItem $ lblBranch gui) [Gtk.widgetVisible Gtk.:= True]
        liftIO $ Gtk.set (getItem $ comboBranch gui) [Gtk.widgetVisible Gtk.:= True]

        -- fill with data
        liftIO $ set (comboBranch gui) otherBranches
        liftIO $ Gtk.comboBoxPrependText (getItem $ comboBranch gui) curBranch
        liftIO $ Gtk.comboBoxSetActive (getItem $ comboBranch gui) 0

        -- register branch switch fn
        config <- ask
        liftIO $ Gtk.on (getItem $ comboBranch gui) Gtk.changed $ changeBranchFn' config (fmap (fromMaybe "") $ get $ comboBranch gui)
        return ()
    where
    changeBranchFn' :: Common.Config -> IO String -> IO ()
    changeBranchFn' cfg branchIO = do
        let (store, view) = getItem $ logTreeView gui
        branch <- branchIO
        newLogEntries <- Common.runVcs cfg $ changeBranchFn branch
        set (logTreeView gui) newLogEntries
        -- set cursor to first log entry (so checkoutbutton works)
        Just firstRowIter <- Gtk.treeModelGetIterFirst store
        firstRow <- Gtk.treeModelGetPath store firstRowIter
        Gtk.treeViewSetCursor view firstRow Nothing


loadLogGui :: [Common.LogEntry] -> Common.Ctx LogGUI
loadLogGui logEntries = do
        liftIO $ do
            gladepath <- getGladepath
            builder <- openGladeFile gladepath
            win <- getWindowFromGlade builder "logWindow"
            revDetails <- getLabelFromGlade builder "lblRevisionDetails"
            actCheck <- getActionFromGlade builder "actCheckout"
            actCanc <- getActionFromGlade builder "actCancel"
            comboBranch <- getComboBoxFromGlade builder "comboBranch"
            lblBranch <- getLabelFromGlade builder "lblBranch"

            treeView <- getTreeViewFromGlade builder "historyTreeView" logEntries
            return $ LogGUI win treeView revDetails actCheck actCanc comboBranch lblBranch























