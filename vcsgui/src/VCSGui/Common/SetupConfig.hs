{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.SetupConfig
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Module for displaying configuration dialogs.
--
-----------------------------------------------------------------------------

module VCSGui.Common.SetupConfig (
    showSetupConfigGUI
) where
import Control.Monad.Trans(liftIO)
import Data.Maybe
import Control.Monad
import System.Directory
import System.Directory(doesDirectoryExist)
import Data.List(findIndex)
import Paths_vcsgui(getDataFileName)

import VCSGui.Common.Error
import qualified VCSGui.Common.GtkHelper as H
import qualified VCSGui.Common.MergeTool as MergeTool
import qualified VCSWrapper.Common as Wrapper
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Control.Applicative ((<$>))
import GI.Gtk.Objects.Action (onActionActivate)
import GI.Gtk.Enums
       (ResponseType(..), FileChooserAction, FileChooserAction(..))
import GI.Gtk.Objects.ToggleButton (onToggleButtonToggled)
import GI.Gtk.Objects.Widget
       (widgetDestroy, widgetHide, widgetShowAll)
import GI.Gtk.Objects.ComboBox (comboBoxSetActive)
import GI.Gtk.Objects.Window
       (setWindowTransientFor, setWindowTitle, Window(..))
import Data.GI.Base (new)
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Objects.Dialog (dialogRun, dialogAddButton)
import GI.Gtk.Interfaces.FileChooser
       (setFileChooserAction, fileChooserGetFilename)
import GI.Gtk.Objects.Builder (Builder(..))

type Config = Maybe (Wrapper.VCSType, Wrapper.Config, Maybe MergeTool.MergeTool)
            --config for setting up vcs

type Callback = (Config -> IO ())

getGladepath = getDataFileName "data/guiCommonSetupRepo.glade"

accessorWindowSetupRepo = "windowSetupRepo"
accessorActOk = "actOk"
accessorActCancel = "actCancel"
accessorActBrowseRepo = "actBrowseRepo"
accessorActBrowseExec = "actBrowseExec"
accessorEntRepo = "entRepo"
accessorEntExec = "entExec"
accessorEntAuthor = "entAuthor"
accessorEntEmail = "entEmail"
accessorEntComboBoxVCSType = "comboBoxVCSType"
accessorCheckbtExec = "checkbtExec"
accessorCheckbtAuthor = "checkbtAuthor"
accessorLblExec = "lblExec"
accessorBtnBrowseExec = "btnBrowseExec"
accessorLblAuthor = "lblAuthor"
accessorLblEmail = "lblEmail"
accessorActBrowsePathToTool = "actBrowsePathToTool"
accessorentPathToTool= "entPathToTool"


data SetupRepoGUI = SetupRepoGUI {
    winSetupRepo :: H.WindowItem
    , actOk :: H.ActionItem
    , actCancel :: H.ActionItem
    , actBrowseRepo :: H.ActionItem
    , actBrowseExec :: H.ActionItem
    , entRepo :: H.TextEntryItem
    , entExec :: H.TextEntryItem
    , entAuthor :: H.TextEntryItem
    , entEmail :: H.TextEntryItem
    , comboBoxVCSType :: H.ComboBoxItem
    , checkbtExec :: H.CheckButtonItem
    , checkbtAuthor :: H.CheckButtonItem
    , lblExec :: H.LabelItem
    , btnBrowseExec :: H.ButtonItem
    , lblAuthor :: H.LabelItem
    , lblEmail :: H.LabelItem
    , entPathToTool :: H.TextEntryItem
    , actBrowsePathToTool :: H.ActionItem
}


{-  | Displays a window to setup a repo. Window will be initially filled with content given config
      is not Nothing. Given callback will be called on successful completition.
-}
showSetupConfigGUI :: Config
        -- ^ maybe a tuple (vcstype,config, mbmergetool), which will be used to fill out the form
        -> Callback  -- ^ callback, called when dialog is closed
        -> IO ()
showSetupConfigGUI mbConfig callback = loadAndOpenWindow
                                                            (loadSetupRepoGui)
                                                            (connectSetupRepoGui callback)
                                                            winSetupRepo
                                                            (initSetupRepoGui mbConfig)


loadSetupRepoGui :: IO SetupRepoGUI
loadSetupRepoGui = loadGuiTemplate $ \builder -> do
    winSetupRepo <- H.getWindowFromGlade builder accessorWindowSetupRepo
    actOk <- H.getActionFromGlade builder accessorActOk
    actCancel <- H.getActionFromGlade builder accessorActCancel
    actBrowseRepo <- H.getActionFromGlade builder accessorActBrowseRepo
    actBrowseExec <- H.getActionFromGlade builder accessorActBrowseExec
    entRepo <- H.getTextEntryFromGlade builder accessorEntRepo
    entExec <- H.getTextEntryFromGlade builder accessorEntExec
    entAuthor <- H.getTextEntryFromGlade builder accessorEntAuthor
    entEmail <- H.getTextEntryFromGlade builder accessorEntEmail
    comboBoxVCSType <- H.getComboBoxFromGlade builder accessorEntComboBoxVCSType
    checkbtExec <- H.getCheckButtonFromGlade builder accessorCheckbtExec
    H.set checkbtExec True
    checkbtAuthor <- H.getCheckButtonFromGlade builder accessorCheckbtAuthor
    H.set checkbtAuthor True
    lblExec <- H.getLabelFromGlade builder accessorLblExec
    btnBrowseExec <- H.getButtonFromGlade builder accessorBtnBrowseExec
    lblAuthor <- H.getLabelFromGlade builder accessorLblAuthor
    lblEmail <- H.getLabelFromGlade builder accessorLblEmail
    entPathToTool <- H.getTextEntryFromGlade builder accessorentPathToTool
    actBrowsePathToTool <- liftIO $ H.getActionFromGlade builder accessorActBrowsePathToTool
    return $ SetupRepoGUI winSetupRepo actOk actCancel actBrowseRepo actBrowseExec entRepo entExec entAuthor entEmail comboBoxVCSType checkbtExec checkbtAuthor lblExec btnBrowseExec lblAuthor lblEmail entPathToTool actBrowsePathToTool

connectSetupRepoGui :: Callback
                    -> SetupRepoGUI
                    -> IO ()
connectSetupRepoGui callback gui = do
        H.registerClose $ winSetupRepo gui
        liftIO $ H.registerCloseAction (actCancel gui) (winSetupRepo gui)

        onActionActivate (H.getItem (actOk gui)) $
                                do mbConfig <- createVCSTypAndConfig gui
                                   case mbConfig of
                                    Left errorMsg ->  showErrorGUI errorMsg
                                    Right tuple   -> do
                                                       callback $ Just tuple
                                                       H.closeWin $ winSetupRepo gui

        onActionActivate (H.getItem (actBrowseRepo gui)) $ liftIO $ do
            mbPath <- showFolderChooserDialog "Choose repository location" (H.getItem $ winSetupRepo gui) FileChooserActionSelectFolder
            case mbPath of
                Nothing -> return ()
                Just path -> do
                    -- discover vcs
                    availableVCS <- discoverVCS path
                    H.set (comboBoxVCSType gui) $ map (T.pack . show) availableVCS
                    -- update gui
                    H.set (entRepo gui) (T.pack path)
                    return ()
        onActionActivate (H.getItem (actBrowseExec gui)) $ liftIO $ do
            mbExec <- showFolderChooserDialog "Choose executable location" (H.getItem $ winSetupRepo gui) FileChooserActionOpen
            case mbExec of
                Nothing -> return ()
                Just exec -> do
                    H.set (entExec gui) (T.pack exec)
                    H.set (checkbtExec gui) True
                    return ()

        onToggleButtonToggled (H.getItem (checkbtExec gui)) $ do
                                            putStrLn "checkbtnexec toogled"
                                            active <- H.get (checkbtExec gui)
                                            if active then do
                                                    widgetShowAll (H.getItem (lblExec gui))
                                                    widgetShowAll (H.getItem (entExec gui))
                                                    widgetShowAll (H.getItem (btnBrowseExec gui))
                                                else do
                                                    widgetHide (H.getItem (lblExec gui))
                                                    widgetHide (H.getItem (entExec gui))
                                                    widgetHide (H.getItem (btnBrowseExec gui))

        onToggleButtonToggled (H.getItem (checkbtAuthor gui)) $ do
                                            putStrLn "checkbtnauthor toogled"
                                            active <- H.get (checkbtAuthor gui)
                                            if active then do
                                                    widgetShowAll (H.getItem (lblAuthor gui))
                                                    widgetShowAll (H.getItem (entAuthor gui))
                                                    widgetShowAll (H.getItem (lblEmail gui))
                                                    widgetShowAll (H.getItem (entEmail gui))
                                                else do
                                                    widgetHide (H.getItem (lblAuthor gui))
                                                    widgetHide (H.getItem (entAuthor gui))
                                                    widgetHide (H.getItem (lblEmail gui))
                                                    widgetHide (H.getItem (entEmail gui))
                                            return ()
        liftIO $ onActionActivate (H.getItem (actBrowsePathToTool gui)) $ do
            mbPath <- showFolderChooserDialog "Select executable" (H.getItem $ winSetupRepo gui) FileChooserActionOpen
            case mbPath of
                Nothing -> return ()
                Just path -> do
                    -- update gui
                    H.set (entPathToTool gui) (T.pack path)
                    return ()
        return ()

    where
    createVCSTypAndConfig :: SetupRepoGUI -> IO (Either Text (Wrapper.VCSType, Wrapper.Config, Maybe MergeTool.MergeTool))
    createVCSTypAndConfig gui = do
            path <- H.get (entRepo gui)


            btExec <- H.get (checkbtExec gui)
            exec <- if btExec then
                        H.get (entExec gui)
                             else
                        return Nothing
            btAuthor <- H.get (checkbtAuthor gui)
            (author,mail) <- if btAuthor then do
                                a <- H.get (entAuthor gui)
                                m <- H.get (entEmail gui)
                                return (a,m)
                                         else
                                return (Nothing,Nothing)
            selectedVCSType <- H.get (comboBoxVCSType gui)
            mbPathToTool <- H.get (entPathToTool gui)
            let tuple = createVCSTypAndConfig' (T.unpack <$> path) author mail (T.unpack <$> exec) (T.unpack <$> selectedVCSType) (T.unpack <$> mbPathToTool)
            putStrLn $ "tuple"++show tuple
            return tuple
            where
                createVCSTypAndConfig' Nothing _ _ _ _ _ = Left "You have to set path and type of VCS. Missing path."
                createVCSTypAndConfig' _ _ _ _ Nothing _ = Left "You have to set path and type of VCS. Missing vcs type."
                createVCSTypAndConfig' _ Nothing (Just _) _ _ _ = Left "If you provide an email you have to provide author as well"
                createVCSTypAndConfig' mbPath@(Just path) authorName email executable (Just vcsType) mbPathToTool= Right $
                                                                            (read vcsType,
                                                                            Wrapper.makeConfig mbPath
                                                                                               executable
                                                                                               $ author authorName,
                                                                            mergeTool mbPathToTool)
                        where
                            author (Just name) =  (Just (Wrapper.Author name email))
                            author Nothing     =  Nothing
                            mergeTool (Just path) = Just $ MergeTool.MergeTool path
                            mergeTool Nothing = Nothing


initSetupRepoGui :: Config
                 -> SetupRepoGUI
                 -> IO ()
initSetupRepoGui mbConfig gui = do
        case mbConfig of
            Nothing -> return ()
            Just (vcsType, Wrapper.Config mbPath mbExec mbAuthor _, mbMergeTool) -> do
                case mbPath of
                    Nothing -> return ()
                    Just path -> do
                                    H.set (entRepo gui) $ T.pack path
                                    availableVCS <- discoverVCS path
                                    H.set (comboBoxVCSType gui) $ map (T.pack . show) availableVCS
                                    --get position (hopefully always index in liststore = index in list)
                                    case findIndex (== vcsType) availableVCS of
                                        Just index ->
                                            --set active
                                            comboBoxSetActive (H.getItem (comboBoxVCSType gui)) (fromIntegral index)
                                        _ -> return ()
                case mbExec of
                    Nothing -> do
                                    H.set (checkbtExec gui) False
                                    return ()
                    Just exec -> do
                                    H.set (checkbtExec gui) True
                                    H.set (entExec gui) $ T.pack exec

                case mbAuthor of
                    Nothing -> do
                                    H.set (checkbtAuthor gui) False
                                    return ()
                    Just (Wrapper.Author author email) -> do
                                    H.set (checkbtAuthor gui) True
                                    H.set (entAuthor gui) author
                                    H.set (entEmail gui) $ fromMaybe "" email
                case mbMergeTool of
                    Nothing -> return()
                    Just mergeTool -> H.set (entPathToTool gui) (T.pack $ MergeTool.fullPath mergeTool)

                return ()

discoverVCS :: FilePath -- ^ path to root
           -> IO [Wrapper.VCSType] -- ^ vcs discovered
discoverVCS path = do
    exists <- doesDirectoryExist path
    putStrLn $ "Path "++ show exists ++ " Starting discovery."
    case exists of
        True -> do
            content <- getDirectoryContents path
            let vcsFiles = map vcsMapping $ filter vcsFilter content
            putStrLn $ "Available vcs: "++ show vcsFiles
            return vcsFiles
        False -> do
            return []
    where
        vcsFilter :: FilePath -> Bool
        vcsFilter path = case path of
                            ".git" -> True
                            ".svn" -> True
                            ".hg"  -> True
                            _      -> False
        vcsMapping :: FilePath -> Wrapper.VCSType
        vcsMapping path = case path of
                            ".git" -> Wrapper.GIT
                            ".svn" -> Wrapper.SVN
                            ".hg"  -> Wrapper.Mercurial
                            _      -> Wrapper.SVN --TODO throw error on this, improve this code

-- | shows a dialog to choose a folder, returns Just FilePath to folder if succesfull, Nothing if cancelled
showFolderChooserDialog :: Text -- ^ title of the window
    -> Window -- ^ parent window
    -> FileChooserAction
    -> IO (Maybe FilePath)
showFolderChooserDialog title parent fcAction = do
    dialog <- new FileChooserDialog []
    setWindowTitle dialog title
    dialogAddButton dialog "gtk-cancel" (fromIntegral $ fromEnum ResponseTypeCancel)
    dialogAddButton dialog "Select" (fromIntegral $ fromEnum ResponseTypeAccept)
    setWindowTransientFor dialog parent
    setFileChooserAction dialog fcAction
    response <- dialogRun dialog
    case toEnum $ fromIntegral response of
        ResponseTypeCancel      -> widgetDestroy dialog >> return Nothing
        ResponseTypeDeleteEvent -> widgetDestroy dialog >> return Nothing
        ResponseTypeAccept      -> do
            f <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return $ Just f



loadAndOpenWindow :: IO SetupRepoGUI -- ^ load gui fn
    -> (SetupRepoGUI -> IO ()) -- ^ connect gui fn
    -> (SetupRepoGUI -> H.WindowItem) -- ^ get WindowItem from gui
    -> (SetupRepoGUI -> IO ()) -- ^ init gui
    -> IO ()
loadAndOpenWindow loadGui connectGui getWindow initRepo = do
    gui <- loadGui
    connectGui gui
    widgetShowAll $ H.getItem (getWindow gui)
    initRepo gui
    return ()


loadGuiTemplate :: (Builder -> IO a) -> IO a
loadGuiTemplate builderFn = do
    gladepath <- getGladepath
    builder <- H.openGladeFile gladepath
    builderFn builder
