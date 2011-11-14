-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.MergeToolGUI
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

module VCSGui.Common.MergeToolGUI (
    showMergeToolGUI
) where

import qualified VCSWrapper.Common as Wrapper
import VCSGui.Common.Types
import qualified VCSGui.Common.GtkHelper as H
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Maybe
import Paths_vcsgui(getDataFileName)
import VCSGui.Common.MergeTool as MT
import VCSGui.Common.Error as Error

--
-- glade path and object accessors
--

getGladepath = getDataFileName "guiCommonMergeTool.glade"
accessorWindowMergeTool = "windowMergeTool"
accessorActBrowsePath = "actBrowsePath"
accessorActCancel = "actCancel"
accessorActUse = "actUse"
accessorEntPath = "entPath"


--
-- types
--

-- callback for ok action
type OkCallBack = MT.MergeToolSetter

data MergeToolGUI = MergeToolGUI {
    windowMergeTool :: H.WindowItem
    , actBrowsePath :: H.ActionItem
    , actCancel :: H.ActionItem
    , actUse :: H.ActionItem
    , entPath :: H.TextEntryItem
}


showMergeToolGUI :: OkCallBack
        -> IO()
showMergeToolGUI okCallback = do
    putStrLn "Starting mergetool gui ..."
    gui <- loadMergeToolGUI

    -- connect actions
    H.registerClose $ windowMergeTool gui
    H.registerCloseAction (actCancel gui) (windowMergeTool gui)
    on (H.getItem (actUse gui)) actionActivated $ do
                                        mbPath <- H.get (entPath gui)
                                        case mbPath of
                                            Nothing -> Error.showErrorGUI
                                                        "You have to choose a merge tool."
                                            Just path -> do
                                                okCallback $ MT.MergeTool path
                                                H.closeWin (windowMergeTool gui)

    on (H.getItem (actBrowsePath gui)) actionActivated $ do
            mbPath <- showFolderChooserDialog "Select executable" (H.getItem $ windowMergeTool gui) FileChooserActionOpen
            case mbPath of
                Nothing -> return ()
                Just path -> do
                    -- update gui
                    H.set (entPath gui) path
                    return ()

    -- present window
    widgetShowAll $ H.getItem $ windowMergeTool gui

    return ()



loadMergeToolGUI :: IO MergeToolGUI
loadMergeToolGUI = do
                gladepath <- liftIO getGladepath
                builder <- H.openGladeFile gladepath
                win <- H.getWindowFromGlade builder accessorWindowMergeTool
                actBrowsePath <- H.getActionFromGlade builder accessorActBrowsePath
                actCancel <-  H.getActionFromGlade builder accessorActCancel
                actUse <-  H.getActionFromGlade builder accessorActUse
                entPath <-  H.getTextEntryFromGlade builder accessorEntPath
                return $ MergeToolGUI win actBrowsePath actCancel actUse entPath

-- HELPER

-- | shows a dialog to choose a folder, returns Just FilePath to folder if succesfull, Nothing if cancelled
showFolderChooserDialog :: String -- ^ title of the window
    -> Window -- ^ parent window
    -> FileChooserAction
    -> IO (Maybe FilePath)
showFolderChooserDialog title parent fcAction = do
    dialog <- fileChooserDialogNew (Just title) (Just parent) fcAction [("Cancel", ResponseCancel), ("Select", ResponseAccept)]
    response <- dialogRun dialog
    case response of
        ResponseCancel      -> widgetDestroy dialog >> return Nothing
        ResponseDeleteEvent -> widgetDestroy dialog >> return Nothing
        ResponseAccept      -> do
            f <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return f




