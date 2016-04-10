{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.ConflictsResolved
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides a VCS-independent GUI to ask a user if conflicts have been resolved.
--
-----------------------------------------------------------------------------

module VCSGui.Common.ConflictsResolved (
    showConflictsResolvedGUI
) where

import qualified VCSWrapper.Common as Wrapper
import Paths_vcsgui(getDataFileName)
import qualified VCSGui.Common.GtkHelper as H
import Control.Monad.Trans(liftIO)
import Control.Monad.Reader(ask)
import GI.Gtk.Objects.Action (onActionActivate)
import GI.Gtk.Objects.Widget (widgetShowAll)

--
-- glade path and object accessors
--

getGladepath = getDataFileName "data/guiCommonConflictsResolved.glade"
accessorWindowConflictsResolved = "windowConflictsResolved"
accessorActConflictsResolved = "actConflictsResolved"
accessorActConflictsNotResolved = "actConflictsNotResolved"


data ConflictsResolvedGUI = ConflictsResolvedGUI {
    windowConflictsResolved :: H.WindowItem
    , actConflictsResolved :: H.ActionItem
    , actConflictsNotResolved :: H.ActionItem
}

-- | Shows a GUI asking user if conflicts have been resolved.
showConflictsResolvedGUI :: (Bool -> Wrapper.Ctx()) -- ^ Handler for user response. Argument is: True if conflicts have been resolved, False if not.
                            -> Wrapper.Ctx ()
showConflictsResolvedGUI handler = do
    liftIO $ putStrLn "Starting conflictsResolvedGUI ..."
    gui <- loadConflictsResolvedGUI

    -- connect actions
    liftIO $ H.registerClose $ windowConflictsResolved gui
    config <- ask
    liftIO $ onActionActivate (H.getItem (actConflictsNotResolved gui)) $ do
                                        Wrapper.runVcs config $ handler False
                                        H.closeWin (windowConflictsResolved gui)

    liftIO $ onActionActivate (H.getItem (actConflictsResolved gui)) $ do
                                        Wrapper.runVcs config $ handler True
                                        H.closeWin (windowConflictsResolved gui)

    -- present window
    liftIO $ widgetShowAll $ H.getItem $ windowConflictsResolved gui

    return ()

loadConflictsResolvedGUI :: Wrapper.Ctx ConflictsResolvedGUI
loadConflictsResolvedGUI = do
                gladepath <- liftIO getGladepath
                builder <- liftIO $ H.openGladeFile gladepath

                win <- liftIO $ H.getWindowFromGlade builder accessorWindowConflictsResolved
                actConflictsResolved <- liftIO $  H.getActionFromGlade builder accessorActConflictsResolved
                actConflictsNotResolved <- liftIO $  H.getActionFromGlade builder accessorActConflictsNotResolved
                return $ ConflictsResolvedGUI win actConflictsResolved actConflictsNotResolved
