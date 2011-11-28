-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.AskPassword
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
-- TODO this should be moved into Common Package
-- | Provides a GUI asking a user for a password.
--
-----------------------------------------------------------------------------

module VCSGui.Svn.AskPassword (
    showAskpassGUI
    ,Handler
) where

import qualified VCSGui.Common.GtkHelper as H
import qualified VCSGui.Common as VCSGUI

import qualified VCSWrapper.Common as Wrapper

import Graphics.UI.Gtk

import Data.Maybe (fromMaybe)
import Control.Monad.Trans(liftIO)
import Paths_vcsgui(getDataFileName)
import Control.Monad.Reader(ask)
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

{- |
    'Handler' is a function used as an argument to the 'showAskpassGUI'. It represents a VCS
    command running in a 'Wrapper.Ctx' expecting password data which can be the followingAxis
        * Nothing, if GUI is closed or cancel button is pressed
        * Just (savePasswordSettingForSession, Maybe password)
    where savePasswordSettingForSession indicates if password-settings should be saved for current
    session. Maybe password is either:
        * Nothing, no password given
        * Just password, password has been provided
-}
type Handler = ((Maybe (Bool, Maybe String))
                -> Wrapper.Ctx())

data AskpassGUI = AskpassGUI {
    windowAskpass :: H.WindowItem
    ,actOk :: H.ActionItem
    ,actCancel :: H.ActionItem
    ,entryPw :: H.TextEntryItem
    ,checkbtUsePw :: H.CheckButtonItem
    ,checkbtSaveForSession :: H.CheckButtonItem
    ,boxUsePwd :: VBox
}

-- | Shows a GUI asking the user for a password. 'Handler' will be called with result.
showAskpassGUI :: Handler -- ^ 'Handler' to be called when GUI is closed passing the result.
                  -> Wrapper.Ctx ()
showAskpassGUI handler = do
    config <- ask
    liftIO $ do
        gui <- loadAskpassGUI

        -- connect actions
        registerClose (windowAskpass gui) handler config
        registerCloseAction (actCancel gui) (windowAskpass gui) handler config
        on (H.getItem (actOk gui)) actionActivated $ do
                                            putStrLn "ok clicked"
                                            usePw <- H.get $ checkbtUsePw gui
                                            pw <- H.get (entryPw gui)
                                            saveForSession <- H.get $ checkbtSaveForSession gui

                                            let args =  if usePw then Just (saveForSession, Just $ fromMaybe "" pw)
                                                                 else Just (saveForSession, Nothing)
                                            VCSGUI.defaultVCSExceptionHandler $ Wrapper.runVcs config $ handler args
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
                -> Handler
                -> Wrapper.Config
                -> IO()
registerClose win handler config = on (H.getItem win) deleteEvent (liftIO (close win handler config) >> return False) >> return ()

registerCloseAction :: H.ActionItem
                    -> H.WindowItem
                    -> Handler
                    -> Wrapper.Config
                    -> IO()
registerCloseAction act win handler config = on (H.getItem act) actionActivated (liftIO (close win handler config)) >> return ()

close :: H.WindowItem
                -> Handler
                -> Wrapper.Config
                -> IO()
close win handler config = do
                        liftIO $ H.closeWin win
                        VCSGUI.defaultVCSExceptionHandler $ Wrapper.runVcs config $ handler Nothing

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
