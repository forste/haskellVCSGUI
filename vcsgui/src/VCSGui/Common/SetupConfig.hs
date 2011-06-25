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

import Monad
import Directory

import VCSGui.Common.Error
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
    , entryPath :: TextEntryItem
    , entryAuthor :: TextEntryItem
    , entryEmail :: TextEntryItem
    , comboBoxVCSType :: ComboBoxItem
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
    entPath <- getTextEntryFromGlade builder "entPath"
    entAuthor <- getTextEntryFromGlade builder "entAuthor"
    entEmail <- getTextEntryFromGlade builder "entEmail"
    comboBoxVCSType <- getComboBoxFromGlade builder "comboBoxVCSType"
    -- init gui with existing repo
    case mbConfig of
        Nothing -> return ()
        Just (Wrapper.Config mbPath _ mbAuthor) -> do
            case mbPath of
                Nothing -> return ()
                Just path -> do
                                set entPath $ path
                                availableVCS <- discoverVCS path
                                set comboBoxVCSType $ map (\vcs -> show vcs) availableVCS
            case mbAuthor of
                Nothing -> return ()
                Just (Wrapper.Author author email) -> do
                                set entAuthor author
                                set entEmail $ fromMaybe "" email
            return ()
    return $ SetupRepoGUI win actOk actCancel actSelRepo entPath entAuthor entEmail comboBoxVCSType

connectSetupRepoGui :: Maybe Wrapper.Config
                    -> (Maybe (Wrapper.VCSType, Wrapper.Config)
                        -> IO ())
                    -> SetupRepoGUI -> IO ()
connectSetupRepoGui mbConfig callback gui = do
        registerClose $ setupRepoWin gui
        liftIO $ registerCloseAction (actCancel gui) (setupRepoWin gui)

        on (getItem (actOk gui)) actionActivated $
                                do mbConfig <- createVCSTypAndConfig gui
                                   case mbConfig of
                                    Left errorMsg ->  showErrorGUI errorMsg
                                    Right tuple   -> do
                                                       callback $ Just tuple
                                                       closeWin $ setupRepoWin gui

        on (getItem (actSelectRepo gui)) actionActivated $ liftIO $ do
            mbPath <- selectRepoWindow "Choose repository location" $ getItem $ setupRepoWin gui
            case mbPath of
                Nothing -> return ()
                Just path -> updateGUI path
        return ()
    where
    updateGUI path = do
        -- discover vcs here
        availableVCS <- discoverVCS path
        set (comboBoxVCSType gui) $ map (\vcs -> show vcs) availableVCS

        -- update gui
        set (entryPath gui) path

        return ()
    createVCSTypAndConfig :: SetupRepoGUI -> IO (Either String (Wrapper.VCSType, Wrapper.Config))
    createVCSTypAndConfig gui = do
--            let path = repoPath gui
            path <- get (entryPath gui)
            author <- get (entryAuthor gui)
            mail <- get (entryEmail gui)
            let executable = Nothing :: Maybe String
            selectedVCSType <- get (comboBoxVCSType gui)
            let tuple = createVCSTypAndConfig' path author mail executable selectedVCSType
            putStrLn $ "tuple"++show tuple
            return tuple
            where
                createVCSTypAndConfig' Nothing _ _ _ _ = Left "You have to set path and type of VCS. Missing path."
                createVCSTypAndConfig' _ _ _ _ Nothing = Left "You have to set path and type of VCS. Missing vcs type."
                createVCSTypAndConfig' _ Nothing (Just _) _ _ = Left "If you provide an email you have to provide author as well"
                createVCSTypAndConfig' mbPath@(Just path) authorName email executable (Just vcsType) = Right $
                                                                            (read vcsType,
                                                                            Wrapper.makeConfig mbPath
                                                                                               executable
                                                                                               $ author authorName)
                        where
                            author (Just name) =  (Just (Wrapper.Author name email))
                            author Nothing     =  Nothing


discoverVCS :: FilePath -- ^ path to root
           -> IO [Wrapper.VCSType] -- ^ vcs discovered
discoverVCS path = do
    content <- getDirectoryContents path
    let vcsFiles = map vcsMapping $ filter vcsFilter content
    return vcsFiles
    where
        vcsFilter :: FilePath -> Bool
        vcsFilter path = case path of
                            ".git" -> True
                            ".svn" -> True
                            _      -> False
        vcsMapping :: FilePath -> Wrapper.VCSType
        vcsMapping path = case path of
                            ".git" -> Wrapper.GIT
                            ".svn" -> Wrapper.SVN
                            _      -> Wrapper.SVN --TODO throw error on this, improve this code

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
