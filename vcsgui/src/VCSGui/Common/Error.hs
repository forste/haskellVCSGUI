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
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Common.Error (
    showErrorGUI
) where
import Graphics.UI.Gtk

showErrorGUI :: String -> IO ()
showErrorGUI msg = do
    dialog <- messageDialogNew Nothing [] MessageError ButtonsOk msg
    _ <- dialogRun dialog
    widgetDestroy dialog
    return ()
