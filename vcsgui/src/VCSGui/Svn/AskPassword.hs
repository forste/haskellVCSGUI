-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.AskPassword
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

module VCSGui.Svn.AskPassword (
    showAskpassGUI
) where

import Graphics.UI.Gtk
import qualified VCSGui.Common.GtkHelper as H
import Data.Maybe (fromMaybe)
import Control.Monad.Trans(liftIO)

import Paths_vcsgui(getDataFileName)
--
-- glade path and object accessors
--
getGladepath = getDataFileName "guiSvnAskpass.glade"
accessorWindow = "windowAskpass"
accessorActOk = "actOk"
accessorActCancel = "actCancel"
accessorEntryPw = "entryPw"
accessorCheckbtUsePw = "checkbtUsePw"
accessorCheckbtSaveForSession = "checkbtSaveForSession"
accessorboxUsePwd = "boxUsePwd"


data AskpassGUI = AskpassGUI {
    windowAskpass :: H.WindowItem
    ,actOk :: H.ActionItem
    ,actCancel :: H.ActionItem
    ,entryPw :: H.TextEntryItem
    ,checkbtUsePw :: H.CheckButtonItem
    ,checkbtSaveForSession :: H.CheckButtonItem
    ,boxUsePwd :: VBox
}

showAskpassGUI :: (Maybe (Maybe (Bool, String)) -- ^ args passed to handler, Nothing if operation is aborted (cancel,quit)
                                                -- ^ else Just Nothing if use password = false, else Just (saveForSession, pw)
                  -> IO ()) -- handler which
                  -> IO ()
showAskpassGUI handler = do
    gui <- loadAskpassGUI

    -- connect actions
    registerClose (windowAskpass gui) handler
    registerCloseAction (actCancel gui) (windowAskpass gui) handler
    on (H.getItem (actOk gui)) actionActivated $ do
                                        putStrLn "ok clicked"
                                        usePw <- H.get $ checkbtUsePw gui
                                        pw <- H.get (entryPw gui)
                                        saveForSession <- H.get $ checkbtSaveForSession gui

                                        let args =  if not usePw then
                                                            Just Nothing
                                                        else
                                                            Just $ Just (saveForSession, fromMaybe "" pw)
                                        handler args
                                        putStrLn $ show args
                                        H.closeWin (windowAskpass gui)
    on (H.getItem (checkbtUsePw gui)) toggled $ do
                                        active <- get (H.getItem (checkbtUsePw gui)) toggleButtonActive
                                        if active then
                                                widgetShowAll (boxUsePwd gui)
                                            else
                                                widgetHideAll (boxUsePwd gui)
    -- present window
    widgetShowAll $ H.getItem $ windowAskpass gui
    return ()

registerClose :: H.WindowItem
                -> (Maybe (Maybe (Bool, String)) -> IO())
                -> IO ()
registerClose win handler = on (H.getItem win) deleteEvent (liftIO (close win handler) >> return False) >> return ()

registerCloseAction :: H.ActionItem -> H.WindowItem ->  (Maybe (Maybe (Bool, String)) -> IO()) -> IO ()
registerCloseAction act win handler = on (H.getItem act) actionActivated (liftIO (close win handler)) >> return ()

close :: H.WindowItem
                -> (Maybe (Maybe (Bool, String)) -> IO())
                -> IO ()
close win handler = do
                        putStrLn "Close called"
                        H.closeWin win
                        handler Nothing

loadAskpassGUI :: IO AskpassGUI
loadAskpassGUI = do
    gladepath <- getGladepath
    builder <- H.openGladeFile gladepath

    windowAskpass <- H.getWindowFromGlade builder accessorWindow
    actOk <- H.getActionFromGlade builder accessorActOk
    actCancel <- H.getActionFromGlade builder accessorActCancel
    entryPw <- H.getTextEntryFromGlade builder accessorEntryPw
    checkbtUsePw <- H.getCheckButtonFromGlade builder accessorCheckbtUsePw
    set (H.getItem checkbtUsePw) [toggleButtonActive := True]
    checkbtSaveForSession <- H.getCheckButtonFromGlade builder accessorCheckbtSaveForSession
    set (H.getItem checkbtSaveForSession) [toggleButtonActive := True]
    boxUsePw <- builderGetObject builder castToVBox accessorboxUsePwd
    return $ AskpassGUI windowAskpass actOk actCancel entryPw checkbtUsePw checkbtSaveForSession boxUsePw
