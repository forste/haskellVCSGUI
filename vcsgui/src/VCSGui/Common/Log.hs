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

) where

import Control.Monad.Reader
import Graphics.UI.Gtk

import VCSGui.Common.GtkHelper
import VCSWrapper.Git
import VCSWrapper.Svn


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
    , logTreeView :: TreeView
    , lblRevisionDetails :: LabelItem
    , actCheckout :: ActionItem
    , actLogCancel :: ActionItem
}


--openLogWindow :: Core.GitRepo -> IO ()
--openLogWindow repo = loadAndOpenWindow (loadLogGui repo) (connectLogGui repo) logWin
--

loadLogGui :: (TreeView -> IO (ListStore a))
            -> [String]
            -> (ListStore a -> IO ())
            -> (a -> String -> Ctx ())
            -> IO LogGUI
loadLogGui setupListStore options doBranchSwitch doCheckout = loadGuiTemplate (\builder -> do
        -- load gui elements
        logWindow <- getWindowFromGlade builder "logWindow"
        treeView <- builderGetObject builder castToTreeView "historyTreeView" -- TODO move this to gtkHelper?
        lblRevisionDetails <- getLabelFromGlade builder "lblRevisionDetails"
        actCheck <- getActionFromGlade builder "actCheckout"
        actCanc <- getActionFromGlade builder "actCancel"
        comboBranch <- getComboBoxFromGlade builder "comboBranch"

        -- init gui
        listStore <- setupListStore treeView

        on (getItem (actCheckout gui)) actionActivated $ doCheckout'  >> (closeWin (logWin gui))

        return $ LogGUI logWindow treeView lblRevisionDetails actCheck actCanc)
    where
    doCheckout' store view combo = doCheckout (do
            (path, _) <- treeViewGetCursor view
            Just treeIter <- treeModelGetIter store path
            selectedLog <- listStoreGetValue store $ listStoreIterToIndex treeIter
            return selectedLog) getGetter comboBranch -- TODO fix IO here...


--
--setupLogEntries :: TreeViewItem Core.LogEntry -> Core.GitLog -> IO ()
--setupLogEntries item (Core.GitLog logs) = do
--    getSetter item logs
--    addTextColumnToTreeView item "Subject" (\Core.LogEntry { Core.subject = t } -> [cellText := t])
--    addTextColumnToTreeView item "Author" (\Core.LogEntry { Core.author = t, Core.email = mail } -> [cellText := (t ++ " <" ++ mail ++ ">")])
--    addTextColumnToTreeView item "Date" (\Core.LogEntry { Core.date = t } -> [cellText := t])
--
--connectLogGui :: Core.GitRepo -> LogGUI -> IO ()
--connectLogGui repo gui = do
--    registerClose $ logWin gui
--    registerCloseAction (actLogCancel gui) (logWin gui)
--    on (getItem (actCheckout gui)) actionActivated $ doCheckout gui repo >> liftIO (closeWin (logWin gui))
--
--    return ()
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





















