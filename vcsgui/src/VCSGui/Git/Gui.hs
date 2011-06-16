-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Gui
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Harald Jagenteufel
-- Stability   :  experimental
-- Portability :
--
--
--
-- TODO catch GitError and display instead of crushing ;)
-----------------------------------------------------------------------------

module VCSGui.Git.Gui (
    main
    , openCommitWindow
    , openErrorWin
    , openRepoWindow
    , openLogWindow
) where
import VCSGui.Common.GtkHelper

import Graphics.UI.Gtk

import qualified VCSWrapper.Git as Core

import System.Directory

import Control.Monad.Trans(liftIO, lift)
import Control.Monad

import Paths_vcsgui(getDataFileName)
import Control.Monad.Reader (ReaderT(..))


getGladepath = getDataFileName "guiGit.glade"


main :: IO ()
main = do
    initGUI
    tmpRepo <- Core.openRepo "/home/ubuntu/testrepo" "Testauthor" "authorEmail"
    openRepoWindow Nothing (\ (Just (Core.GitRepo p a e))  -> putStrLn ("ok called!" ++ p ++ a ++ e))
    mainGUI




------------------------------------------------
-- LOG VIEWER / CHECKOUT
------------------------------------------------

data LogGUI = LogGUI {
    logWin :: WindowItem
    , logTreeView :: TreeViewItem Core.LogEntry
    , lblRevisionDetails :: LabelItem
    , actCheckout :: ActionItem
    , actLogCancel :: ActionItem
}

openLogWindow :: Core.GitRepo -> IO ()
openLogWindow repo = loadAndOpenWindow (loadLogGui repo) (connectLogGui repo) logWin

loadLogGui :: Core.GitRepo -> IO LogGUI
loadLogGui repo = loadGuiTemplate (\builder -> do
    logWindow <- getWindowFromGlade builder "logWindow"
    treeView <- getTreeViewFromGlade builder "historyTreeView" ([] :: [Core.LogEntry])
    lblRevisionDetails <- getLabelFromGlade builder "lblRevisionDetails"
    Just log <- Core.simpleLog repo
    setupLogEntries treeView log
    actCheck <- getActionFromGlade builder "actCheckout"
    actCanc <- getActionFromGlade builder "actCancel"
    return $ LogGUI logWindow treeView lblRevisionDetails actCheck actCanc)

setupLogEntries :: TreeViewItem Core.LogEntry -> Core.GitLog -> IO ()
setupLogEntries item (Core.GitLog logs) = do
    getSetter item logs
    addTextColumnToTreeView item "Subject" (\Core.LogEntry { Core.subject = t } -> [cellText := t])
    addTextColumnToTreeView item "Author" (\Core.LogEntry { Core.author = t, Core.email = mail } -> [cellText := (t ++ " <" ++ mail ++ ">")])
    addTextColumnToTreeView item "Date" (\Core.LogEntry { Core.date = t } -> [cellText := t])

connectLogGui :: Core.GitRepo -> LogGUI -> IO ()
connectLogGui repo gui = do
    registerClose $ logWin gui
    registerCloseAction (actLogCancel gui) (logWin gui)
    on (getItem (actCheckout gui)) actionActivated $ doCheckout gui repo >> liftIO (closeWin (logWin gui))

    return ()

doCheckout :: LogGUI -> Core.GitRepo -> IO ()
doCheckout gui repo = do
    let (store, view) = getItem $ logTreeView gui
    (path, _) <- treeViewGetCursor view
    Just treeIter <- treeModelGetIter store path
    selectedValue <- listStoreGetValue store $ listStoreIterToIndex treeIter
    putStrLn $ "checking out rev: " ++ (Core.commitID selectedValue)
    Core.checkout repo $ Core.commitID selectedValue



------------------------------------------------
-- SETUP REPO
------------------------------------------------

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
openRepoWindow :: Maybe Core.GitRepo -- ^ maybe an already configured repo
    -> (Maybe Core.GitRepo -> IO ()) -- ^ called when dialog is closed
    -> IO ()
openRepoWindow mbRepo repoSetter = loadAndOpenWindow (loadSetupRepoGui mbRepo) (connectSetupRepoGui mbRepo repoSetter) setupRepoWin

loadSetupRepoGui :: Maybe Core.GitRepo -> IO SetupRepoGUI
loadSetupRepoGui mbRepo = loadGuiTemplate (\builder -> do
    win <- getWindowFromGlade builder "setupRepoWindow"
    actOk <- getActionFromGlade builder "actOk"
    actSelRepo <- getActionFromGlade builder "actSelectRepo"
    actInitRepo <- getActionFromGlade builder "actInitRepo"
    lblRepoPath <- getLabelFromGlade builder "lblRepoPath"
    entAuthor <- getTextEntryFromGlade builder "entAuthor"
    entEmail <- getTextEntryFromGlade builder "entEmail"
    case mbRepo of
        Nothing -> return $ SetupRepoGUI win actOk actSelRepo actInitRepo lblRepoPath entAuthor entEmail Nothing
        Just (Core.GitRepo path _ _) -> return $ SetupRepoGUI win actOk actSelRepo actInitRepo lblRepoPath entAuthor entEmail (Just path)
    )

connectSetupRepoGui :: Maybe Core.GitRepo -> (Maybe Core.GitRepo -> IO ()) -> SetupRepoGUI -> IO ()
connectSetupRepoGui mbRepo repoSetter gui = do
        -- init gui with existing repo
        case mbRepo of
            Nothing -> return ()
            Just (Core.GitRepo path author email) -> do
                getSetter (lblRepoPath gui) path
                getSetter (entryAuthor gui) author
                getSetter (entryEmail gui) email
                return ()

        -- register handlers
        registerClose $ setupRepoWin gui
        registerOkAct gui

        on (getItem (actInitRepo gui)) actionActivated $ liftIO (do
            mbPath <- selectRepoWindow "Choose location for new repository" (getItem (setupRepoWin gui))
            case mbPath of
                Nothing -> return ()
                Just path -> do
                    (Core.GitRepo path _ _) <- Core.initRepo path "" ""
                    registerNewGuiOnOkAct path
                    )

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
        getSetter (lblRepoPath newGui) path

        -- register actOk with new gui
        registerOkAct newGui
        return ()


-- TODO how to use maybe monad here? don't use IO here!
extractRepo :: SetupRepoGUI -> IO (Maybe Core.GitRepo)
extractRepo gui = do
    case (repoPath gui) of
        Nothing -> return Nothing
        Just p -> do
            author <- getGetter (entryAuthor gui)
            case author of
                Nothing -> return Nothing
                Just a -> do
                    mail <- getGetter (entryEmail gui)
                    case mail of
                        Nothing -> return Nothing
                        Just m -> do
                            r <- Core.openRepo p a m
                            return $ Just r


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

------------------------------------------------
-- COMMIT
------------------------------------------------

data CommitGUI = CommitGUI {
    commitWin :: WindowItem
    , commitMsg :: TextViewItem
    , actCommit :: ActionItem
    , commitFiles :: TreeViewItem SelectableCommitFile
    , toggleRenderer :: CellRendererToggle
    , actCancel :: ActionItem
}

data SelectableCommitFile = SCFile {
    selected :: Bool
    , path :: FilePath
    , hint :: String -- ^ status hint: is this file modified, added, removed, untracked,...
} deriving (Show)


-- TODO add parent as parameter? add author
openCommitWindow :: Core.GitRepo -> IO ()
openCommitWindow repo = loadAndOpenWindow (loadCommitGui repo) (connectCommitGui repo) commitWin


loadCommitGui :: Core.GitRepo -> IO CommitGUI
loadCommitGui repo = loadGuiTemplate (\builder -> do
    commitWin <- getWindowFromGlade builder "commitWindow"
    commitMsg <- getTextViewFromGlade builder "txtCommitMsg"
    actCommit <- getActionFromGlade builder "actCommit"
    listViewItem <- getTreeViewFromGlade builder "treeViewCommitFiles" ([] :: [SelectableCommitFile])
    repoStatus <- Core.status repo
    toggleRenderer <- setupCommitFiles listViewItem repoStatus
    actCancel <- getActionFromGlade builder "actCancel"

    return $ CommitGUI commitWin commitMsg actCommit listViewItem toggleRenderer actCancel)


setupCommitFiles :: TreeViewItem SelectableCommitFile -> Core.GitStatus -> IO CellRendererToggle
setupCommitFiles item repoStatus = do
    let modFiles = map (\file -> SCFile { selected = True, path = file, hint = "modified" }) (Core.modifiedFiles repoStatus)
        addFiles = map (\file -> SCFile { selected = True, path = file, hint = "added" }) (Core.addedFiles repoStatus)
        untrackFiles = map (\file -> SCFile { selected = False, path = file, hint = "untracked" }) (Core.untrackedFiles repoStatus)
    getSetter item (modFiles ++ addFiles ++ untrackFiles)

    toggleRenderer <- cellRendererToggleNew
    addColumnToTreeView item toggleRenderer "Commit" (\SCFile { selected = s } -> [cellToggleActive := s])

    addTextColumnToTreeView item "File" (\SCFile { path = p } -> [cellText := p])

    addTextColumnToTreeView item "File status" (\SCFile { hint = h } -> [cellText := h])

    return toggleRenderer


connectCommitGui :: Core.GitRepo -> CommitGUI -> IO ()
connectCommitGui repo gui = do
    registerClose $ commitWin gui
    on (getItem (actCancel gui)) actionActivated $ closeWin (commitWin gui)
    on (getItem (actCommit gui)) actionActivated $ doCommit gui repo >> closeWin (commitWin gui)
    on (toggleRenderer gui) cellToggled $ \filepath -> do
        putStrLn ("toggle called: " ++ filepath)
        let (_, (store, _), _) = commitFiles gui
        Just treeIter <- treeModelGetIterFromString store filepath
        value <- listStoreGetValue store $ listStoreIterToIndex treeIter
        let newValue = value { selected = not (selected value) }
        listStoreSetValue store (listStoreIterToIndex treeIter) newValue
        return ()
    return ()


doCommit :: CommitGUI -> Core.GitRepo -> IO ()
doCommit gui repo = do
    Just commitMsg <- getGetter $ commitMsg gui
    Just listedFiles <- getGetter $ commitFiles gui
    let selectedFiles = map (\SCFile { path = p} -> p ) $ filter (\SCFile { selected = s } -> s) listedFiles

    Core.commit repo selectedFiles commitMsg


------------------------------------------------
-- MISC
------------------------------------------------

openErrorWin :: String -> IO ()
openErrorWin msg = do
    dialog <- messageDialogNew Nothing [] MessageError ButtonsOk msg
    _ <- dialogRun dialog
    widgetDestroy dialog
    return ()


------------------------------------------------
-- HELPERS
------------------------------------------------

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

registerClose :: WindowItem -> IO ()
registerClose win = on (getItem win) deleteEvent (liftIO (closeWin win) >> return False) >> return ()

registerCloseAction :: ActionItem -> WindowItem -> IO ()
registerCloseAction act win = on (getItem act) actionActivated (liftIO (closeWin win)) >> return ()

closeWin :: WindowItem -> IO ()
closeWin win = (widgetHideAll (getItem win))

