-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.Log
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

getGladepath = getDataFileName "guiGit.glade"

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


showLogGUI :: [Common.LogEntry] -- ^ logEntries to be displayed initially
            -> [String] -- ^ options will be displayed in a menu as checkboxes TODO implement
            -> Maybe ((String, [String]), -- ^ list of branchnames to display
                (String -> Common.Ctx [Common.LogEntry])) -- ^ called when a different branch is selected. gets the new branchname
            -> (Common.LogEntry -- ^ selected line
                -> (Maybe String) -- ^ name of the branch to checkout from
                -> Common.Ctx ()) -- ^ called on checkout action. will close window afterwards
            -> Common.Ctx ()
showLogGUI logEntries options Nothing doCheckoutFn = guiWithoutBranches logEntries options doCheckoutFn >> return ()
showLogGUI logEntries options (Just (branches, changeBranchFn)) doCheckoutFn = do
        gui <- guiWithoutBranches logEntries options doCheckoutFn
        guiAddBranches gui branches changeBranchFn
        return ()

guiWithoutBranches :: [Common.LogEntry]
                    -> [String]
                    -> (Common.LogEntry
                        -> (Maybe String)
                        -> Common.Ctx ())
                    -> Common.Ctx LogGUI
guiWithoutBranches logEntries options doCheckoutFn = do
        gui <- loadLogGui logEntries
        liftIO $ setupLogColumns gui

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
        (path, _) <- Gtk.treeViewGetCursor view
        Just treeIter <- Gtk.treeModelGetIter store path
        selectedLog <- Gtk.listStoreGetValue store $ Gtk.listStoreIterToIndex treeIter
        selectedBranch <- get combo
        Common.runVcs cfg $ doCheckoutFn selectedLog selectedBranch

    setupLogColumns :: LogGUI -> IO ()
    setupLogColumns gui = do
        let item = (logTreeView gui)
        addTextColumnToTreeView item "Subject" (\Common.LogEntry { Common.subject = t } -> [Gtk.cellText Gtk.:= t])
        addTextColumnToTreeView item "Author" (\Common.LogEntry { Common.author = t, Common.email = mail } -> [Gtk.cellText Gtk.:= (t ++ " <" ++ mail ++ ">")])
        addTextColumnToTreeView item "Date" (\Common.LogEntry { Common.date = t } -> [Gtk.cellText Gtk.:= t])
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
        liftIO $ Gtk.on (getItem $ comboBranch gui) Gtk.changed $ changeBranchFn' config (getItem $ logTreeView gui) (fmap (fromMaybe "") $ get $ comboBranch gui)
        return ()
    where
    changeBranchFn' :: Common.Config -> (Gtk.ListStore Common.LogEntry, Gtk.TreeView) -> IO String -> IO ()
    changeBranchFn' cfg (store, _) branchIO = do
        branch <- branchIO
        newLogEntries <- Common.runVcs cfg $ changeBranchFn branch
        set (logTreeView gui) newLogEntries

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


-- TODO move this methods to helper?

closeWin :: WindowItem -> IO ()
closeWin win = (Gtk.widgetHideAll (getItem win))

registerClose :: WindowItem -> IO ()
registerClose win = Gtk.on (getItem win) Gtk.deleteEvent (liftIO (closeWin win) >> return False) >> return ()

registerCloseAction :: ActionItem -> WindowItem -> IO ()
registerCloseAction act win = Gtk.on (getItem act) Gtk.actionActivated (liftIO (closeWin win)) >> return ()






















