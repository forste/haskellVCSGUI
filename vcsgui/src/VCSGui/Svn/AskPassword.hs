{-# LANGUAGE OverloadedStrings #-}
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

import Data.Maybe (fromMaybe)
import Control.Monad.Trans(liftIO)
import Paths_vcsgui(getDataFileName)
import Control.Monad.Reader(ask)
import Data.Text (Text)
import GI.Gtk.Objects.VBox (VBox(..))
import GI.Gtk.Objects.Action (onActionActivate)
import GI.Gtk.Objects.ToggleButton
       (setToggleButtonActive, getToggleButtonActive,
        onToggleButtonToggled)
import GI.Gtk.Objects.Widget
       (onWidgetDeleteEvent, widgetHide, widgetShowAll)
import Data.GI.Base.Attributes (AttrOp(..))
import GI.Gtk.Objects.Builder (builderGetObject)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
--
-- glade path and object accessors
--
getGladepath = getDataFileName "data/guiSvnAskpass.glade"
accessorWindow = "windowAskpass"
accessorActOk = "actOk"
accessorActCancel = "actCancel"
accessorEntryPw = "entryPw"
accessorCheckbtUsePw = "checkbtUsePw"
accessorCheckbtSaveForSession = "checkbtSaveForSession"
accessorboxUsePwd = ("boxUsePwd" :: Text)

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
type Handler = ((Maybe (Bool, Maybe Text))
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
        onActionActivate (H.getItem (actOk gui)) $ do
                                            putStrLn "ok clicked"
                                            usePw <- H.get $ checkbtUsePw gui
                                            pw <- H.get (entryPw gui)
                                            saveForSession <- H.get $ checkbtSaveForSession gui

                                            let args =  if usePw then Just (saveForSession, Just $ fromMaybe "" pw)
                                                                 else Just (saveForSession, Nothing)
                                            VCSGUI.defaultVCSExceptionHandler $ Wrapper.runVcs config $ handler args
                                            H.closeWin (windowAskpass gui)
        onToggleButtonToggled (H.getItem (checkbtUsePw gui)) $ do
                                            active <- getToggleButtonActive (H.getItem (checkbtUsePw gui))
                                            if active then
                                                    widgetShowAll (boxUsePwd gui)
                                                else
                                                    widgetHide (boxUsePwd gui)
        -- present window
        widgetShowAll $ H.getItem $ windowAskpass gui
        return ()

registerClose :: H.WindowItem
                -> Handler
                -> Wrapper.Config
                -> IO()
registerClose win handler config = onWidgetDeleteEvent (H.getItem win) (\e -> liftIO (close win handler config) >> return False) >> return ()

registerCloseAction :: H.ActionItem
                    -> H.WindowItem
                    -> Handler
                    -> Wrapper.Config
                    -> IO()
registerCloseAction act win handler config = onActionActivate (H.getItem act) (liftIO (close win handler config)) >> return ()

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
    setToggleButtonActive (H.getItem checkbtUsePw) True
    checkbtSaveForSession <- H.getCheckButtonFromGlade builder accessorCheckbtSaveForSession
    setToggleButtonActive (H.getItem checkbtSaveForSession) True
    boxUsePw <- builderGetObject builder accessorboxUsePwd >>= unsafeCastTo VBox
    return $ AskpassGUI windowAskpass actOk actCancel entryPw checkbtUsePw checkbtSaveForSession boxUsePw
