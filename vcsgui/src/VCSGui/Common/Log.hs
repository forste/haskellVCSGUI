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
import VCSWrapper.Git
import VCSWrapper.Svn


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
    , logTreeView :: TreeViewItem LogEntry
    , lblRevisionDetails :: LabelItem
    , actCheckout :: ActionItem
    , actLogCancel :: ActionItem
    , comboBranch :: ComboBoxItem
}


--openLogWindow :: Core.GitRepo -> IO ()
--openLogWindow repo = loadAndOpenWindow (loadLogGui repo) (connectLogGui repo) logWin
--

newLogGui :: IO [LogEntry] -- ^ logEntries to be displayed initially
            -> [String] -- ^ options will be displayed in a menu as checkboxes TODO implement
            -> Maybe (ListStore LogEntry -> IO String -> IO ()) -- ^ called when a different branch is selected
            -> (IO LogEntry -- ^ selected line
                -> IO (Maybe String) -- ^ name of the branch to checkout from
                -> IO ()) -- ^ TODO change a to revision? called on checkout action. will close window afterwards
            -> IO ()
newLogGui logEntries _ mbDoBranchSwitch doCheckout = do
        logs <- logEntries
        gui <- loadLogGui logs

        -- connect gui elements
        registerClose $ logWin gui
        registerCloseAction (actLogCancel gui) (logWin gui)

        on (getItem (actCheckout gui)) actionActivated $
            doCheckout' (logTreeView gui) (comboBranch gui)
                >> (closeWin (logWin gui))

        return ()
    where
    doCheckout' :: TreeViewItem LogEntry -> ComboBoxItem -> IO ()
    doCheckout' (_, (store, view), _) combo = doCheckout (do
            (path, _) <- treeViewGetCursor view
            Just treeIter <- treeModelGetIter store path
            selectedLog <- listStoreGetValue store $ listStoreIterToIndex treeIter
            return selectedLog) (getGetter combo)


loadLogGui :: [LogEntry] -> IO LogGUI
loadLogGui logEntries = do
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




--
--doCheckout :: LogGUI -> Core.GitRepo -> IO ()
--doCheckout gui repo = do
--    let (store, view) = getItem $ logTreeView gui
--    (path, _) <- treeViewGetCursor view
--    Just treeIter <- treeModelGetIter store path
--    selectedValue <- listStoreGetValue store $ listStoreIterToIndex treeIter
--    putStrLn $ "checking out rev: " ++ (Core.commitID selectedValue)
--    Core.checkout repo $ Core.commitID selectedValue





--loadGui branches branchHandler listStoreSetter checkOutAction = do
        --    logWindow <- getWindowFromGlade builder "logWindow"
        --    treeView <- getTreeViewFromGlade builder "historyTreeView" ([] :: [Core.LogEntry])
        --    lblRevisionDetails <- getLabelFromGlade builder "lblRevisionDetails"

--            listStoreSetter treeView

--            if empty branches then setInvisible branchLabel+dropDownList (or change vbox)
--            else on select branches branchHandler --somehow pass liststore to handler so he can refill it



--            on buttonActivated checkoutButton $ do
--               checkOutAction listStore --pass selected branch if not invisible
--





















