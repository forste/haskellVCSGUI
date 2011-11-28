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
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Common.ConflictsResolved (
    showConflictsResolvedGUI
) where

import qualified VCSWrapper.Common as Wrapper
import Paths_vcsgui(getDataFileName)
import qualified VCSGui.Common.GtkHelper as H
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Control.Monad.Reader(ask)

--
-- glade path and object accessors
--

getGladepath = getDataFileName "guiCommonConflictsResolved.glade"
accessorWindowConflictsResolved = "windowConflictsResolved"
accessorActConflictsResolved = "actConflictsResolved"
accessorActConflictsNotResolved = "actConflictsNotResolved"


data ConflictsResolvedGUI = ConflictsResolvedGUI {
    windowConflictsResolved :: H.WindowItem
    , actConflictsResolved :: H.ActionItem
    , actConflictsNotResolved :: H.ActionItem
}

showConflictsResolvedGUI :: (Bool -> Wrapper.Ctx())
                            -> Wrapper.Ctx ()
showConflictsResolvedGUI handler = do
    liftIO $ putStrLn "Starting conflictsResolvedGUI ..."
    gui <- loadConflictsResolvedGUI

    -- connect actions
    liftIO $ H.registerClose $ windowConflictsResolved gui
    config <- ask
    liftIO $ on (H.getItem (actConflictsNotResolved gui)) actionActivated $ do
                                        Wrapper.runVcs config $ handler False
                                        H.closeWin (windowConflictsResolved gui)

    liftIO $ on (H.getItem (actConflictsResolved gui)) actionActivated $ do
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
