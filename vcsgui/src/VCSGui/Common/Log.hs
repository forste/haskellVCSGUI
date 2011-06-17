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

data LogConfig a = LogConfig {
    options :: String
    ,treeViewSetter :: TreeView -> TreeViewItem a
    }

-- don't use state, use
-- use state monad
-- e.g. mainLog :: [Log] -> BranchHandler -> IO()
-- type BranchHandler = (Label -> Dropbox -> ListModel -> IO ())
-- mainLog logs handler = do
--                              on dropSelect $ do
--                                                  put branch
--                              on checkoutButtonPress $ do
--                                                  get branch

--data LogCtx a b = LogCtx (ReaderT (LogConfig b) IO a)
--   deriving (Monad)
--   , MonadIO b, MonadReader (LogConfig b) b)

--usage e.g. runWithConfig $ loadGui "path/to/repo"
--  where
--      runWithConfig = runLogCtx curConfig
--      runLogCtx config (LogCtx a) = runReaderT a config
--      curConfig = makeConfig options ...

--loadGui :: FilePath -> LogCtx ()
--loadGui path = loadGuiTemplate \builder ->
--    logWindow <- getWindowFromGlade builder "logWindow"
--    treeView <- getTreeViewFromGlade builder "historyTreeView" ([] :: [Core.LogEntry])
--    lblRevisionDetails <- getLabelFromGlade builder "lblRevisionDetails"

--    config <- ask
--

--    Just log <- Core.simpleLog repo
--    setupLogEntries treeView log

--    actCheck <- getActionFromGlade builder "actCheckout"
--    actCanc <- getActionFromGlade builder "actCancel"
--    return $ LogGUI logWindow treeView lblRevisionDetails actCheck actCanc)
