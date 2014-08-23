{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
--  TODO since this window has to print the entered password to stdout, nothing else must be printed to stdout. can we fix this? (logging)
-- | A simple window to enter a password. Prints the password to stdout (as required for GIT_ASKPASS).
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Graphics.UI.Gtk
import qualified VCSGui.Common.GtkHelper as H
import Data.Maybe (fromMaybe)

import Paths_vcsgui(getDataFileName)
import qualified Data.Text as T (unpack)
--
-- glade path and object accessors
--
getGladepath = getDataFileName "data/guiCommonAskpass.glade"
accessorWindow = "windowAskpass"
accessorActOk = "actOk"
accessorActCancel = "actCancel"
accessorEntryPw = "entryPw"


data AskpassGUI = AskpassGUI {
    window :: H.WindowItem
    ,actOk :: H.ActionItem
    ,actCancel :: H.ActionItem
    ,entryPw :: H.TextEntryItem
}


main :: IO ()
main = do
    initGUI
    showAskpassGUI
    mainGUI


showAskpassGUI :: IO ()
showAskpassGUI = do
    gui <- loadCommitGUI

    -- connect actions
    H.registerQuit $ window gui
    H.registerQuitAction $ actCancel gui
    on (H.getItem (actOk gui)) actionActivated $ do
                                        pw <- H.get (entryPw gui)
                                        putStr . T.unpack $ fromMaybe "" pw
                                        mainQuit
    -- present window
    widgetShowAll $ H.getItem $ window gui
    return ()


loadCommitGUI :: IO AskpassGUI
loadCommitGUI = do
    gladepath <- getGladepath
    builder <- H.openGladeFile gladepath

    win <- H.getWindowFromGlade builder accessorWindow
    actOk <- H.getActionFromGlade builder accessorActOk
    actCancel <- H.getActionFromGlade builder accessorActCancel
    entryPw <- H.getTextEntryFromGlade builder accessorEntryPw
    return $ AskpassGUI win actOk actCancel entryPw

