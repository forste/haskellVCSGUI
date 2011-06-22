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
import Data.Maybe

import VCSGui.Common.GtkHelper
import qualified VCSWrapper.Common as Wrapper

import Paths_vcsgui(getDataFileName)
getGladepath = getDataFileName "guiCommonSetupRepo.glade"


data SetupRepoGUI = SetupRepoGUI {
    setupRepoWin :: WindowItem
    , actOk :: ActionItem
    , actCancel :: ActionItem
    , actSelectRepo :: ActionItem
--    , actInitRepo :: ActionItem
    , lblRepoPath :: LabelItem
    , entryAuthor :: TextEntryItem
    , entryEmail :: TextEntryItem
    , repoPath :: Maybe FilePath
}


-- example call:
-- do
--  getAvailableshowSetupConfig

-- TODO check if repo exists and what kind
--      fill combobox/liststore with discovered types
--      get type on okAction and pass it to callback (along with config)
showSetupConfigGUI :: Maybe Wrapper.Config -- ^ maybe a config
                    -> (Maybe (Wrapper.VCSType, Wrapper.Config)  -- ^ Just (VCSType,Config) if everything is set correctly
                        -> IO ())           -- ^ called when dialog is closed
                    -> IO ()
showSetupConfigGUI mbConfig callback = loadAndOpenWindow
                                                            (loadSetupRepoGui mbConfig)
                                                            (connectSetupRepoGui mbConfig callback)
                                                            setupRepoWin


loadSetupRepoGui :: Maybe Wrapper.Config -> IO SetupRepoGUI
loadSetupRepoGui mbConfig = loadGuiTemplate $ \builder -> do
    win <- getWindowFromGlade builder "setupRepoWindow"
    actOk <- getActionFromGlade builder "actOk"
    actCancel <- getActionFromGlade builder "actCancel"
    actSelRepo <- getActionFromGlade builder "actSelectRepo"
--    actInitRepo <- getActionFromGlade builder "actInitRepo"
    lblRepoPath <- getLabelFromGlade builder "lblRepoPath"
    entAuthor <- getTextEntryFromGlade builder "entAuthor"
    entEmail <- getTextEntryFromGlade builder "entEmail"
    let path = case mbConfig of
                    Nothing -> Nothing
                    Just (Wrapper.Config p _ _) -> p
    return $ SetupRepoGUI win actOk actCancel actSelRepo lblRepoPath entAuthor entEmail path

connectSetupRepoGui :: Maybe Wrapper.Config
                    -> (Maybe (Wrapper.VCSType, Wrapper.Config)
                        -> IO ())
                    -> SetupRepoGUI -> IO ()
connectSetupRepoGui mbConfig callback gui = do
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
        liftIO $ registerCloseAction (actCancel gui) (setupRepoWin gui)

        registerOkAct gui

--        on (getItem (actInitRepo gui)) actionActivated $ liftIO (do
--            mbPath <- selectRepoWindow "Choose location for new repository" (getItem (setupRepoWin gui))
--            let (Wrapper.Config path _ _) = Wrapper.initNewRepo mbPath Nothing Nothing -- TODO implement this
--            registerNewGuiOnOkAct $ fromMaybe "" path
--            )

        on (getItem (actSelectRepo gui)) actionActivated $ liftIO $ do
            mbPath <- selectRepoWindow "Choose repository location" $ getItem $ setupRepoWin gui
            case mbPath of
                Nothing -> return ()
                Just path -> updateGUI path
        return ()
    where
    updateGUI path = do
        -- update gui
        let newGui = gui { repoPath = Just path }
        set (lblRepoPath newGui) path

        -- register actOk with new gui
        registerOkAct newGui
        return ()
    registerOkAct gui = on (getItem (actOk gui)) actionActivated $
                                do mbConfig <- createVCSTypAndConfig gui
                                   callback $ mbConfig
                                   closeWin $ setupRepoWin gui
    createVCSTypAndConfig :: SetupRepoGUI -> IO (Maybe (Wrapper.VCSType, Wrapper.Config))
    createVCSTypAndConfig gui = do
            let path = repoPath gui
            author <- get (entryAuthor gui)
            mail <- get (entryEmail gui)
            let executable = Nothing :: Maybe String
            let vcstype = Wrapper.SVN   -- TODO get type on okAction from combobox/liststore
            return $ createVCSTypAndConfig' path author mail executable vcstype
    createVCSTypAndConfig' :: Maybe FilePath -> Maybe String -> Maybe String -> Maybe String -> Wrapper.VCSType
                     -> (Maybe (Wrapper.VCSType, Wrapper.Config))
    createVCSTypAndConfig' Nothing Nothing Nothing Nothing _ = Nothing
    createVCSTypAndConfig' Nothing _ _ _ _ = Nothing --TODO repoPath not filled out but other fields are (partly) => throw error message so gui can handle it ?
    createVCSTypAndConfig' _ Nothing (Just _) _ _ = Nothing --TODO author not filled out but email is => throw error message so gui can handle it ?
    createVCSTypAndConfig' mbPath@(Just path) authorName email executable vcstype = Just $
                                                                (vcstype,
                                                                Wrapper.makeConfig mbPath
                                                                                   executable
                                                                                   $ author authorName)
            where
                author (Just name) =  (Just (Wrapper.Author name email))
                author Nothing     =  Nothing


-- | shows a dialog to choose a folder, returns Just FilePath to folder if succesfull, Nothing if cancelled
selectRepoWindow :: String -- ^ title of the window
    -> Window -- ^ parent window
    -> IO (Maybe FilePath)
selectRepoWindow title parent = do
    dialog <- fileChooserDialogNew (Just title) (Just parent) FileChooserActionSelectFolder [("Cancel", ResponseCancel), ("Select", ResponseAccept)]
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
