{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
module VCSGui.Common.Log (
    showLogGUI
) where

import Control.Monad.Reader
import Data.Maybe

import VCSGui.Common.GtkHelper
import qualified VCSWrapper.Common as Common


import Data.Maybe (fromMaybe)
import Paths_vcsgui(getDataFileName)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Monoid ((<>))
import qualified GI.Gtk.Objects.TreeView as Gtk
       (treeViewSetCursor, treeViewGetCursor, TreeView(..))
import qualified GI.Gtk.Objects.Action as Gtk (onActionActivate)
import qualified GI.Gtk.Objects.Widget as Gtk
       (setWidgetVisible, widgetShowAll)
import qualified GI.Gtk.Interfaces.TreeModel as Gtk
       (treeModelGetPath, treeModelGetIterFirst, treeModelGetIter)
import qualified Data.GI.Gtk.ModelView.SeqStore as Gtk
       (seqStoreIterToIndex, seqStoreGetValue, SeqStore(..))
import qualified Data.GI.Base.Attributes as Gtk (AttrOp(..))
import qualified Data.GI.Gtk.ComboBox as Gtk
       (comboBoxSetActive, comboBoxPrependText)
import qualified GI.Gtk.Objects.ComboBox as Gtk (onComboBoxChanged)
import GI.Gtk.Objects.TreeViewColumn (noTreeViewColumn)
import Data.GI.Base.Attributes (AttrLabelProxy(..))

_text = AttrLabelProxy :: AttrLabelProxy "text"

getGladepath = getDataFileName "data/guiCommonLog.glade"

data LogConfig a = LogConfig {
    options :: [Text]
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
            -> [Text]
            -- ^ options will be displayed in a menu as checkboxes (TODO this is currently not implemented)
            -> Maybe ((Text, [Text]), (Text -> Common.Ctx [Common.LogEntry]))
            -- ^ (list of branchnames to display, Function called when a different branch is selected)
            --
            -- The function will be called with the selected branchname to repopulate the displayed LogEntries.
            -- If 'Nothing', no branch selection will be displayed.
            -> (Common.LogEntry -> Maybe Text -> Common.Ctx ())
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
                    -> [Text]
                    -> (Common.LogEntry
                        -> (Maybe Text)
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
        liftIO $ Gtk.onActionActivate (getItem (actCheckout gui)) $
            doCheckout' config (logTreeView gui) (comboBranch gui)
                >> (closeWin (logWin gui))

        -- show window
        liftIO $ Gtk.widgetShowAll $ getItem $ logWin gui
        return gui
    where
    doCheckout' :: Common.Config -> TreeViewItem Common.LogEntry -> ComboBoxItem -> IO ()
    doCheckout' cfg (_, (store, view), _) combo = do
        (mbPath, _) <- Gtk.treeViewGetCursor view
        case mbPath of
            Just path -> do
                (True, treeIter) <- Gtk.treeModelGetIter store path
                selectedLog <- Gtk.seqStoreGetValue store =<< Gtk.seqStoreIterToIndex treeIter
                selectedBranch <- get combo
                Common.runVcs cfg $ doCheckoutFn selectedLog selectedBranch
            Nothing -> return () -- TODO fix nothing selected bug here

    setupLogColumns :: LogGUI -> Bool -> IO ()
    setupLogColumns gui displayBranchNames = do
        let item = (logTreeView gui)
        addTextColumnToTreeView item "Subject" (\Common.LogEntry { Common.subject = t } -> [_text Gtk.:= t])
        addTextColumnToTreeView item "Author" (\Common.LogEntry { Common.author = t, Common.email = mail } -> [_text Gtk.:= t <> " <" <> mail <> ">"])
        addTextColumnToTreeView item "Date" (\Common.LogEntry { Common.date = t } -> [_text Gtk.:= t])
        case displayBranchNames of
            True -> addTextColumnToTreeView item "Branch" (\Common.LogEntry { Common.mbBranch = t } -> [_text Gtk.:= fromMaybe "" t])
            False -> return()
        return ()

guiAddBranches :: LogGUI -> (Text, [Text]) -> (Text -> Common.Ctx [Common.LogEntry]) -> Common.Ctx ()
guiAddBranches gui (curBranch, otherBranches) changeBranchFn = do
        -- set branch selection visible
        liftIO $ Gtk.setWidgetVisible (getItem $ lblBranch gui) True
        liftIO $ Gtk.setWidgetVisible (getItem $ comboBranch gui) True

        -- fill with data®
        liftIO $ set (comboBranch gui) otherBranches
        liftIO $ Gtk.comboBoxPrependText (getItem $ comboBranch gui) curBranch
        liftIO $ Gtk.comboBoxSetActive (getItem $ comboBranch gui) 0

        -- register branch switch fn
        config <- ask
        liftIO $ Gtk.onComboBoxChanged (getItem $ comboBranch gui) $ changeBranchFn' config (fmap (fromMaybe "") $ get $ comboBranch gui)
        return ()
    where
    changeBranchFn' :: Common.Config -> IO Text -> IO ()
    changeBranchFn' cfg branchIO = do
        let (store, view) = getItem $ logTreeView gui
        branch <- branchIO
        newLogEntries <- Common.runVcs cfg $ changeBranchFn branch
        set (logTreeView gui) newLogEntries
        -- set cursor to first log entry (so checkoutbutton works)
        Gtk.treeModelGetIterFirst store >>= \case
            (True, firstRowIter) -> do
                firstRow <- Gtk.treeModelGetPath store firstRowIter
                Gtk.treeViewSetCursor view firstRow noTreeViewColumn False
            _ -> return ()


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























