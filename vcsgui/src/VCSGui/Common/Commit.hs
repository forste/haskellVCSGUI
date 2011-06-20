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
-- | TODO this module needs refactoring. showGUI should use GtkHelper to avoid code redundance
-- | TODO helpers (end of file) should be moved to GtkHelper
--
-----------------------------------------------------------------------------
module VCSGui.Common.Commit (
    SCFile(..)
    ,Option
    ,showCommitGUI
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
isLocked _                   = False

type Option = String

-- loads gui objects and connects them
showCommitGUI :: (TreeView -> Wrapper.Ctx (ListStore SCFile))   -- ^ function to set listStore model for treeview
        -> (   String
            -> [FilePath]
            -> [Option]
            -> Wrapper.Config
            -> IO())            -- ^ callback for ok action
        -> String               -- ^ author
        -> FilePath             -- ^ glade
        -> GTKObjectAccessors   -- ^ accessors for gtk objects
        -> Wrapper.Ctx()
showCommitGUI setUpTreeView okCallback author gladepath gtkAccessors = do
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

    -- connect actions
    liftIO $ on commitDialog deleteEvent $ liftIO $ quit commitDialog >> return False
    liftIO $ on actCancel actionActivated $ quit commitDialog >> return ()
    config <- ask
    liftIO $ on actCommit actionActivated $ do
                                        msg <- getTextFromBuffer bufferCommitMsg
                                        selectedFiles <- getSelectedFiles listStore
                                        okCallback msg selectedFiles [] config
                                        quit commitDialog

    -- present window and start main loop
    liftIO $ windowPresent commitDialog
    liftIO $ mainGUI

    liftIO $ putStrLn "Finished"
    return ()

----
---- HELPERS
----
quit :: Dialog -> IO ()
quit commitDialog  = do
        widgetDestroy commitDialog
        liftIO mainQuit

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
