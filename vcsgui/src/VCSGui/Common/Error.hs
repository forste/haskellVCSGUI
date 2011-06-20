-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.Error
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
