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

