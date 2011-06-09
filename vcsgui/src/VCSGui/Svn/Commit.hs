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
-- | TODO DEPRECATED
--
-----------------------------------------------------------------------------
module VCSGui.Svn.Commit (
    showGUI
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Control.Monad.Trans(liftIO)
import Control.Monad

import VCSGui.Common.Types
import qualified VCSWrapper.Svn as Svn

-- data types
data SCFile = SCFile {
        selected :: Bool
        ,path :: FilePath
        ,status :: String
        ,locked :: Bool
    }

-- loads gui objects and connects them
showGUI :: String               -- author
        -> FilePath             -- current working directory
        -> FilePath             -- glade
        -> GTKObjectAccessors   -- accessors for gtk objects
        -> IO()
showGUI cwd author gladepath gtkAccessors = do
    putStrLn "Starting gui ..."
    initGUI

    -- create and load builder
    builder <- builderNew
    builderAddFromFile builder gladepath

    -- retrieve gtk objects
    commitDialog <- builderGetObject builder castToDialog (gtkCommitDialog gtkAccessors)
    actCommit <- builderGetObject builder castToAction (gtkActCommit gtkAccessors)
    actCancel <- builderGetObject builder castToAction (gtkActCancel gtkAccessors)
    bufferCommitMsg <- builderGetObject builder castToTextBuffer (gtkBufferCommitMsg gtkAccessors)
    listView <- builderGetObject builder castToTreeView (gtkListView gtkAccessors)
    btUnlockTargets <- builderGetObject builder castToCheckButton (gtkBtUnlockTargets gtkAccessors)

    -- build and set model
    repoStatus <- runWithConfig $ Svn.status []

    -- create model
    listStore <- listStoreNew [
            (SCFile {
                        selected = ctxSelect (Svn.modification status)
                        ,path = Svn.file status
                        ,status = show (Svn.modification status)
                        ,locked = Svn.isLocked status
                        })
            | status <- repoStatus]
    treeViewSetModel listView listStore

    -- selection column
    selectedPathColumn <- treeViewColumnNew
    set selectedPathColumn [treeViewColumnTitle := "Files to commit" ]
    treeViewAppendColumn listView selectedPathColumn

    -- render selection
    selectedRenderer <- cellRendererToggleNew
    treeViewColumnPackStart selectedPathColumn selectedRenderer False
    cellLayoutSetAttributes selectedPathColumn selectedRenderer listStore $
        \SCFile { selected = s } -> [cellToggleActive := s]

    -- render path
    pathRenderer <- cellRendererTextNew
--    pathRenderer { cellMode = CellEditable }
    treeViewColumnPackEnd selectedPathColumn pathRenderer True
    cellLayoutSetAttributes selectedPathColumn pathRenderer listStore $
        \SCFile { path = p } -> [cellText := p]

    -- status column
    statusColumn <- treeViewColumnNew
    set statusColumn [treeViewColumnTitle := "Status"]
    treeViewAppendColumn listView statusColumn

    -- render status
    statusRenderer <- cellRendererTextNew
    treeViewColumnPackEnd statusColumn statusRenderer False
    cellLayoutSetAttributes statusColumn statusRenderer listStore $
        \SCFile { status = s } -> [cellText := s]

    -- lock column
    lockColumn <- treeViewColumnNew
    set lockColumn [treeViewColumnTitle := "Locked" ]
    treeViewAppendColumn listView lockColumn

    -- render lock
    lockRenderer <- cellRendererToggleNew
    treeViewColumnPackEnd lockColumn lockRenderer False
    cellLayoutSetAttributes lockColumn lockRenderer listStore $
        \SCFile { locked = l } -> [cellToggleActive := l]

    -- connect actions
    on commitDialog deleteEvent $ liftIO mainQuit >> return False
    on actCancel actionActivated $ quit >> return ()
    on actCommit actionActivated $ do
            putStrLn "Commit"
            msg <- getTextFromBuffer bufferCommitMsg
            active <- get btUnlockTargets toggleButtonActive
            selectedFiles <- getSelectedFiles listStore
            let unlockOption = if active then [] else ["--no-unlock"]
            runWithConfig $ Svn.commit selectedFiles author msg unlockOption
            liftIO mainQuit

--    on btUnlockTargets toggled $ do
--                            putStrLn "btUnlockTargets toggled"
--                            active <- get btUnlockTargets toggleButtonActive
--                            set btUnlockTargets [ toggleButtonActive := (not active)]
--                            putStrLn "btUnlockTargets toggled"
--                            return ()

    on selectedRenderer cellToggled $ \columnId -> do
                            Just treeIter <- treeModelGetIterFromString listStore columnId
                            value <- listStoreGetValue listStore $ listStoreIterToIndex treeIter
                            newValue <- createNewValue runWithConfig value
                            listStoreSetValue listStore (listStoreIterToIndex treeIter) newValue
                            return ()

    -- present window and start main loop
    windowPresent commitDialog
    mainGUI

    putStrLn "Finished"
    return ()
    where
        ctxSelect status =  status == Svn.Added || status == Svn.Deleted || status==Svn.Modified ||
                            status==Svn.Replaced
        runWithConfig = Svn.runSvn $ Svn.makeConfig (Just cwd) Nothing Nothing

-- helper

createNewValue :: (Svn.Ctx () -> IO ()) -- adder, needed if file needs to be added <=> is untracked
                -> SCFile -- old value
                -> IO SCFile
createNewValue runWithConfig (SCFile False file "Untracked" isLocked) = do
                            runWithConfig $ Svn.add [file] []
                            return SCFile { selected = True,
                                            path = file,
                                            status = "Added",
                                            locked=isLocked
                                            }
createNewValue _ value = do
            return SCFile { selected = not (selected value),
                            path = (path value),
                            status = (status value)
                            , locked = (locked value)}

getTextFromBuffer :: TextBuffer -> IO String
getTextFromBuffer buffer = do
        (start, end) <- textBufferGetBounds buffer
        textBufferGetText buffer start end False

getSelectedFiles :: ListStore SCFile -> IO [FilePath]
getSelectedFiles listStore = do
            listedFiles <- listStoreToList listStore
            let selectedFiles = map (\SCFile { path = p} -> p )
                                $ filter (\SCFile { selected = s } -> s) listedFiles
            return (selectedFiles)

quit :: IO ()
quit = liftIO mainQuit

createBuilder :: FilePath -> IO Builder
createBuilder filePath =
     do
    builder <- builderNew :: IO Builder
    builderAddFromFile builder filePath :: IO()
    return (builder)
