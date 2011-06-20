-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.SetupConfig
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

module VCSGui.Common.SetupConfig (
    showSetupConfigGUI
) where
import Graphics.UI.Gtk hiding (set, get)
import Control.Monad.Trans(liftIO)
import Data.Maybe(fromMaybe)

import VCSGui.Common.GtkHelper
import qualified VCSWrapper.Common as Wrapper

import Paths_vcsgui(getDataFileName)
getGladepath = getDataFileName "guiGit.glade"


data SetupRepoGUI = SetupRepoGUI {
    setupRepoWin :: WindowItem
    , actOk :: ActionItem
    , actSelectRepo :: ActionItem
    , actInitRepo :: ActionItem
    , lblRepoPath :: LabelItem
    , entryAuthor :: TextEntryItem
    , entryEmail :: TextEntryItem
    , repoPath :: Maybe FilePath
}

-- TODO check if repo exists
showSetupConfigGUI :: Maybe Wrapper.Config -- ^ maybe a config
    -> (Maybe Wrapper.Config -> IO ()) -- ^ called when dialog is closed
    -> IO ()
showSetupConfigGUI mbConfig repoSetter = loadAndOpenWindow (loadSetupRepoGui mbConfig) (connectSetupRepoGui mbConfig repoSetter) setupRepoWin

loadSetupRepoGui :: Maybe Wrapper.Config -> IO SetupRepoGUI
loadSetupRepoGui mbConfig = loadGuiTemplate (\builder -> do
    win <- getWindowFromGlade builder "setupRepoWindow"
    actOk <- getActionFromGlade builder "actOk"
    actSelRepo <- getActionFromGlade builder "actSelectRepo"
    actInitRepo <- getActionFromGlade builder "actInitRepo"
    lblRepoPath <- getLabelFromGlade builder "lblRepoPath"
    entAuthor <- getTextEntryFromGlade builder "entAuthor"
    entEmail <- getTextEntryFromGlade builder "entEmail"
    case mbConfig of
        Nothing -> return $ SetupRepoGUI win actOk actSelRepo actInitRepo lblRepoPath entAuthor entEmail Nothing
        Just (Wrapper.Config path _ _) -> return $ SetupRepoGUI win actOk actSelRepo actInitRepo lblRepoPath entAuthor entEmail path
    )

connectSetupRepoGui :: Maybe Wrapper.Config -> (Maybe Wrapper.Config -> IO ()) -> SetupRepoGUI -> IO ()
connectSetupRepoGui mbConfig repoSetter gui = do
        -- init gui with existing repo
        case mbConfig of
            Nothing -> return ()
            Just (Wrapper.Config mbPath _ mbAuthor) -> do
                set (lblRepoPath gui) $ fromMaybe "" mbPath
                case mbAuthor of
                    Nothing -> return ()
                    Just (Wrapper.Author author email) -> do
                        set (entryAuthor gui) author
                        set (entryEmail gui) $ fromMaybe "" email
                return ()

        -- register handlers
        registerClose $ setupRepoWin gui
        registerOkAct gui

--        on (getItem (actInitRepo gui)) actionActivated $ liftIO (do
--            mbPath <- selectRepoWindow "Choose location for new repository" (getItem (setupRepoWin gui))
--            let (Wrapper.Config path _ _) = Wrapper.initNewRepo mbPath Nothing Nothing -- TODO implement this
--            registerNewGuiOnOkAct $ fromMaybe "" path
--            )

        on (getItem (actSelectRepo gui)) actionActivated $ liftIO (do
            mbPath <- selectRepoWindow "Choose repository location" (getItem (setupRepoWin gui))
            case mbPath of
                Nothing -> return ()
                Just path -> registerNewGuiOnOkAct path
                )
        return ()
    where
    registerOkAct gui = on (getItem (actOk gui)) actionActivated $ (extractRepo gui >>=
                            repoSetter >>
                            closeWin (setupRepoWin gui))
    registerNewGuiOnOkAct path = do
        let newGui = gui { repoPath = Just path }
        set (lblRepoPath newGui) path

        -- register actOk with new gui
        registerOkAct newGui
        return ()


-- TODO how to use maybe monad here? don't use IO here!
extractRepo :: SetupRepoGUI -> IO (Maybe Wrapper.Config)
extractRepo gui = do
        author <- get (entryAuthor gui)
        case author of
            Nothing -> return $ Just $Wrapper.makeConfig (repoPath gui) Nothing Nothing
            Just a -> do
                mail <- get (entryEmail gui)
                return $ Just $ Wrapper.makeConfig (repoPath gui) Nothing (Just (Wrapper.Author a mail))


selectRepoWindow :: String -- ^ Title of the window
    -> Window -- ^ parent window
    -> IO (Maybe FilePath)
selectRepoWindow title parent = do
    dialog <- fileChooserDialogNew (Just title) (Just parent) FileChooserActionSelectFolder [("Cancel", ResponseCancel), ("Open", ResponseAccept)]
    response <- dialogRun dialog
    case response of
        ResponseCancel      -> widgetDestroy dialog >> return Nothing
        ResponseDeleteEvent -> widgetDestroy dialog >> return Nothing
        ResponseAccept      -> do
            f <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return f



loadAndOpenWindow :: IO gui -- ^ load gui fn
    -> (gui -> IO ()) -- ^ connect gui fn
    -> (gui -> WindowItem) -- ^ get WindowItem from gui
    -> IO ()
loadAndOpenWindow loadGui connectGui getWindow = do
    gui <- loadGui
    connectGui gui
    widgetShowAll $ getItem (getWindow gui)
    return ()


loadGuiTemplate :: (Builder -> IO a) -> IO a
loadGuiTemplate builderFn = do
    gladepath <- getGladepath
    builder <- openGladeFile gladepath
    builderFn builder
