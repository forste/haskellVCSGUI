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
    newLogGui
) where

import Control.Monad.Reader
import Graphics.UI.Gtk
import Data.Maybe

import VCSGui.Common.GtkHelper
import qualified VCSWrapper.Common as Common


import Data.Maybe (fromMaybe)
import Paths_vcsgui(getDataFileName)

getGladepath = getDataFileName "guiGit.glade"

data LogConfig a = LogConfig {
    options :: [String]
    ,treeViewSetter :: TreeView -> TreeViewItem a
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
}


--openLogWindow :: Core.GitRepo -> IO ()
--openLogWindow repo = loadAndOpenWindow (loadLogGui repo) (connectLogGui repo) logWin
--

newLogGui :: [Common.LogEntry] -- ^ logEntries to be displayed initially
            -> [String] -- ^ options will be displayed in a menu as checkboxes TODO implement
            -> Maybe ([String], -- ^ list of branchnames to display
                (ListStore Common.LogEntry -> IO String -> Common.Ctx [Common.LogEntry])) -- ^ called when a different branch is selected TODO implement
            -> (Common.LogEntry -- ^ selected line
                -> (Maybe String) -- ^ name of the branch to checkout from
                -> Common.Ctx ()) -- ^ called on checkout action. will close window afterwards
            -> Common.Ctx ()
newLogGui logEntries _ mbDoBranchSwitch doCheckout = do
        gui <- loadLogGui logEntries

        liftIO $ setupLogColumns gui

        -- connect gui elements
        liftIO $ registerClose $ logWin gui
        liftIO $ registerCloseAction (actLogCancel gui) (logWin gui)

        config <- ask
        liftIO $ on (getItem (actCheckout gui)) actionActivated $
            doCheckout' config (logTreeView gui) (comboBranch gui)
                >> (closeWin (logWin gui))

        liftIO $ widgetShowAll $ getItem $ logWin gui
        return ()
    where
    doCheckout' :: Common.Config -> TreeViewItem Common.LogEntry -> ComboBoxItem -> IO ()
    doCheckout' cfg (_, (store, view), _) combo = do
        (path, _) <- treeViewGetCursor view
        Just treeIter <- treeModelGetIter store path
        selectedLog <- listStoreGetValue store $ listStoreIterToIndex treeIter
        selectedBranch <- getGetter combo
        Common.runVcs cfg $ doCheckout selectedLog selectedBranch
    setupLogColumns :: LogGUI -> IO ()
    setupLogColumns gui = do
        let item = (logTreeView gui)
        addTextColumnToTreeView item "Subject" (\Common.LogEntry { Common.subject = t } -> [cellText := t])
        addTextColumnToTreeView item "Author" (\Common.LogEntry { Common.author = t, Common.email = mail } -> [cellText := (t ++ " <" ++ mail ++ ">")])
        addTextColumnToTreeView item "Date" (\Common.LogEntry { Common.date = t } -> [cellText := t])
        return ()


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

            treeView <- getTreeViewFromGlade builder "historyTreeView" logEntries
            return $ LogGUI win treeView revDetails actCheck actCanc comboBranch


-- TODO move this methods to helper?

closeWin :: WindowItem -> IO ()
closeWin win = (widgetHideAll (getItem win))

registerClose :: WindowItem -> IO ()
registerClose win = on (getItem win) deleteEvent (liftIO (closeWin win) >> return False) >> return ()

registerCloseAction :: ActionItem -> WindowItem -> IO ()
registerCloseAction act win = on (getItem act) actionActivated (liftIO (closeWin win)) >> return ()






















