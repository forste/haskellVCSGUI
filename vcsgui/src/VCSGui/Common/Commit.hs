-----------------------------------------------------------------------------
--
-- Module      :  Main
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
module VCSGui.Common.Commit (
    SCFile(..)
    ,Option
    ,showGUI
    ,selected
    ,filePath
    ,status
    ,isLocked
) where

import qualified VCSWrapper.Common as Wrapper
import VCSGui.Common.Types
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Maybe

--TODO add copy ctor for SCFile, similar to createNewValue just changing selected status
data SCFile = GITSCFile Bool FilePath String |
              SVNSCFile Bool FilePath String Bool
    deriving (Show)

selected :: SCFile -> Bool
selected (GITSCFile s _ _) = s
selected (SVNSCFile s _ _ _) = s

filePath :: SCFile -> FilePath
filePath (GITSCFile _ fp _ ) = fp
filePath (SVNSCFile _ fp _ _) = fp

status :: SCFile -> String
status (GITSCFile _ _ s) = s
status (SVNSCFile _ _ s _) = s

isLocked :: SCFile -> Bool
isLocked (SVNSCFile _ _ _ l) = l
--
--
---- TODO add parent as parameter? add author
--openCommitWindow :: Core.GitRepo -> IO ()
--openCommitWindow repo = loadAndOpenWindow (loadCommitGui repo) (connectCommitGui repo) commitWin
--


--type OKCallback = IO ()

--SVN OK CALLBACK

--svnOkCallback :: String -- buffer text
--                -> IO ()
--svnOkCallback bufferText options selectedFiles = do


--setUPTreeView :: TreeView -> IO()
type Option = String

-- loads gui objects and connects them
showGUI :: (TreeView -> Wrapper.Ctx (ListStore SCFile))   -- ^ function to setup treeview
        -> (   String
            -> [FilePath]
            -> [Option]
            -> Wrapper.Config
            -> IO())            -- ^ callback for ok action
        -> String               -- ^ author
        -> FilePath             -- ^ glade
        -> GTKObjectAccessors   -- ^ accessors for gtk objects
        -> Wrapper.Ctx()
showGUI setUpTreeView okCallback author gladepath gtkAccessors = do
    liftIO $ putStrLn "Starting gui ..."
    liftIO $ initGUI

    -- create and load builder
    builder <- liftIO $ builderNew
    liftIO $ builderAddFromFile builder gladepath

    -- retrieve gtk objects
    commitDialog <- liftIO $ builderGetObject builder castToDialog (gtkCommitDialog gtkAccessors)
    actCommit <- liftIO $ builderGetObject builder castToAction (gtkActCommit gtkAccessors)
    actCancel <- liftIO $ builderGetObject builder castToAction (gtkActCancel gtkAccessors)
    bufferCommitMsg <- liftIO $ builderGetObject builder castToTextBuffer (gtkBufferCommitMsg gtkAccessors)
    listView <- liftIO $ builderGetObject builder castToTreeView (gtkListView gtkAccessors)
    btUnlockTargets <- liftIO $ builderGetObject builder castToCheckButton (gtkBtUnlockTargets gtkAccessors)

    listStore <- setUpTreeView listView
    -- build and set model
--    repoStatus <- runWithConfig $ Svn.status []

    -- connect actions
    liftIO $ on commitDialog deleteEvent $ liftIO $ quit commitDialog >> return False
    liftIO $ on actCancel actionActivated $ quit commitDialog >> return ()
    config <- ask
    liftIO $ on actCommit actionActivated $ do
                                        msg <- getTextFromBuffer bufferCommitMsg
                                        selectedFiles <- getSelectedFiles listStore
                                        okCallback msg selectedFiles [] config
                                        quit commitDialog
--            putStrLn $ "Commiting to: "++cwd
--            msg <- getTextFromBuffer bufferCommitMsg
--            active <- get btUnlockTargets toggleButtonActive
--            selectedFiles <- getSelectedFiles listStore
--            let unlockOption = if active then [] else ["--no-unlock"]
--            runWithConfig $ Svn.commit selectedFiles author msg unlockOption
--            quit commitDialog
--            return ()



    -- present window and start main loop
    liftIO $ windowPresent commitDialog
    liftIO $ mainGUI

    liftIO $ putStrLn "Finished"
    return ()
--    where
--        ctxSelect status =  status == Svn.Added || status == Svn.Deleted || status==Svn.Modified ||
--                            status==Svn.Replaced
--        runWithConfig = Svn.runSvn $ Svn.makeConfig (Just cwd) Nothing Nothing

----
---- HELPERS
----
quit :: Dialog -> IO ()
quit commitDialog  = do
        widgetDestroy commitDialog
        liftIO mainQuit

--createNewValue :: (Svn.Ctx () -> IO ()) -- adder, needed if file needs to be added <=> is untracked
--                -> SCFile -- old value
--                -> IO SCFile
--createNewValue runWithConfig (SCFile False file "Untracked" isLocked) = do
--                            runWithConfig $ Svn.add [file] []
--                            return SCFile { selected = True,
--                                            path = file,
--                                            status = "Added",
--                                            locked=isLocked
--                                            }
--createNewValue _ value = do
--            return SCFile { selected = not (selected value),
--                            path = (path value),
--                            status = (status value)
--                            , locked = (locked value)}

getTextFromBuffer :: TextBuffer -> IO String
getTextFromBuffer buffer = do
        (start, end) <- textBufferGetBounds buffer
        textBufferGetText buffer start end False

getSelectedFiles :: ListStore SCFile -> IO [FilePath]
getSelectedFiles listStore = do
            listedFiles <- listStoreToList listStore
            let selectedFiles = map (\scf -> filePath scf )
                                $ filter (\scf -> selected scf) listedFiles
            return (selectedFiles)


















--loadCommitGui :: Core.GitRepo -> IO CommitGUI
--loadCommitGui repo = do
--    -- create and load builder
--    builder <- builderNew
--    builderAddFromFile builder gladepath

--    -- retrieve gtk objects
--    commitWin <- getWindowFromGlade builder "commitWindow"
--    commitMsg <- getTextViewFromGlade builder "txtCommitMsg"
--    actCommit <- getActionFromGlade builder "actCommit"

--    listView <- builderGetObject builder castToTreeView (gtkListView gtkAccessors)

--

--    listViewItem <- getTreeViewFromGlade builder "treeViewCommitFiles" ([] :: [SelectableCommitFile])
--    repoStatus <- Core.status repo
--    toggleRenderer <- setupCommitFiles listViewItem repoStatus

--    actCancel <- getActionFromGlade builder "actCancel"
--
--    return $ CommitGUI commitWin commitMsg actCommit listViewItem toggleRenderer actCancel)
--
--
--setupCommitFiles :: TreeViewItem SelectableCommitFile -> Core.GitStatus -> IO CellRendererToggle
--setupCommitFiles item repoStatus = do
--    let modFiles = map (\file -> SCFile { selected = True, path = file, hint = "modified" }) (Core.modifiedFiles repoStatus)
--        addFiles = map (\file -> SCFile { selected = True, path = file, hint = "added" }) (Core.addedFiles repoStatus)
--        untrackFiles = map (\file -> SCFile { selected = False, path = file, hint = "untracked" }) (Core.untrackedFiles repoStatus)
--    getSetter item (modFiles ++ addFiles ++ untrackFiles)
--
--    toggleRenderer <- cellRendererToggleNew
--    addColumnToTreeView item toggleRenderer "Commit" (\SCFile { selected = s } -> [cellToggleActive := s])
--
--    addTextColumnToTreeView item "File" (\SCFile { path = p } -> [cellText := p])
--
--    addTextColumnToTreeView item "File status" (\SCFile { hint = h } -> [cellText := h])
--
--    return toggleRenderer
--
--
--connectCommitGui :: Core.GitRepo -> CommitGUI -> IO ()
--connectCommitGui repo gui = do
--    registerClose $ commitWin gui
--    on (getItem (actCancel gui)) actionActivated $ closeWin (commitWin gui)
--    on (getItem (actCommit gui)) actionActivated $ doCommit gui repo >> closeWin (commitWin gui)
--    on (toggleRenderer gui) cellToggled $ \filepath -> do
--        putStrLn ("toggle called: " ++ filepath)
--        let (_, (store, _), _) = commitFiles gui
--        Just treeIter <- treeModelGetIterFromString store filepath
--        value <- listStoreGetValue store $ listStoreIterToIndex treeIter
--        let newValue = value { selected = not (selected value) }
--        listStoreSetValue store (listStoreIterToIndex treeIter) newValue
--        return ()
--    return ()
--
--
--doCommit :: CommitGUI -> Core.GitRepo -> IO ()
--doCommit gui repo = do
--    Just commitMsg <- getGetter $ commitMsg gui
--    Just listedFiles <- getGetter $ commitFiles gui
--    let selectedFiles = map (\SCFile { path = p} -> p ) $ filter (\SCFile { selected = s } -> s) listedFiles
--
--    Core.commit repo selectedFiles commitMsg




--------------------------------------------------
---- MISC
--------------------------------------------------
--
--openErrorWin :: String -> IO ()
--openErrorWin msg = do
--    dialog <- messageDialogNew Nothing [] MessageError ButtonsOk msg
--    _ <- dialogRun dialog
--    widgetDestroy dialog
--    return ()

--------------------------------------------------
---- HELPERS
--------------------------------------------------
--
--loadAndOpenWindow :: IO gui -- ^ load gui fn
--    -> (gui -> IO ()) -- ^ connect gui fn
--    -> (gui -> WindowItem) -- ^ get WindowItem from gui
--    -> IO ()
--loadAndOpenWindow loadGui connectGui getWindow = do
--    gui <- loadGui
--    connectGui gui
--    widgetShowAll $ getItem (getWindow gui)
--    return ()
--
--
--loadGuiTemplate :: (Builder -> IO a) -> IO a
--loadGuiTemplate builderFn = do
--    gladepath <- getGladepath
--    builder <- openGladeFile gladepath
--    return builder
--    builderFn builder
--
--registerClose :: WindowItem -> IO ()
--registerClose win = on (getItem win) deleteEvent (liftIO (closeWin win) >> return False) >> return ()
--
--registerCloseAction :: ActionItem -> WindowItem -> IO ()
--registerCloseAction act win = on (getItem act) actionActivated (liftIO (closeWin win)) >> return ()
--
--closeWin :: WindowItem -> IO ()
--closeWin win = (widgetHideAll (getItem win))



---- loads gui objects and connects them
--showGUI :: String               -- author
--        -> FilePath             -- current working directory
--        -> FilePath             -- glade
--        -> GTKObjectAccessors   -- accessors for gtk objects
--        -> VCSType              -- version control type
--        -> IO()
--showGUI cwd author gladepath gtkAccessors SVN = return ()
----showGUI cwd author gladepath gtkAccessors SVN = do
----    Svn.showGUI cwd author gladepath gtkAccessors
--
--showGUI listStore options
--
--
----data CheckoutCtx a = CheckoutCtx (ReaderT CheckoutConfig IO a)
----   deriving (Monad)
----   , MonadIO b, MonadReader CheckoutConfig b)
--
----usage e.g. runWithConfig $ loadGui "path/to/repo"
----  where
----      runWithConfig = runCtx curConfig
----      runCtx config (Checkout a) = runReaderT a config
----      curConfig = makeConfig options ...
--
----loadGui :: FilePath -> CheckoutCtx ()
