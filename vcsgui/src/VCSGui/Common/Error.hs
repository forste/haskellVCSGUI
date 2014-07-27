{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.Error
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Functions to handle errors are found in this module.
--
-----------------------------------------------------------------------------

module VCSGui.Common.Error (
    showErrorGUI
) where

import Graphics.UI.Gtk
import Data.Text (Text)

-- | Displays a simple window displaying given 'String' as an error message.
showErrorGUI :: Text -- ^ Message to display.
    -> IO ()
showErrorGUI msg = do
    dialog <- messageDialogNew Nothing [] MessageError ButtonsOk msg
    _ <- dialogRun dialog
    widgetDestroy dialog
    return ()
