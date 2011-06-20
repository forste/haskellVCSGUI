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
-- | TODO testing, refactor helpers, clean up
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
import VCSGui.Common.GtkHelper
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Maybe
import Paths_vcsgui(getDataFileName)

--
-- glade path and object accessors
--

getGladepath = getDataFileName "guiGit.glade"
accessorCommitWin = "windowCommit"
accessorTreeViewFiles = "treeViewFiles"
accessorActCommit = "actCommit"
accessorActCancel = "actCancel"
accessorActTxtViewMsg = "txtViewMsg"

--
-- types
--

data CommitGUI = LogGUI {
    commitWin :: WindowItem
    , treeViewFiles :: TreeViewItem SCFile
    , actCommit :: ActionItem
    , actCancel :: ActionItem
    , txtViewMsg :: TextViewItem
}

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
showCommitGUI :: (TreeView -> Wrapper.Ctx (ListStore SCFile))   -- ^ fn to set listStore model for treeview
        -> (   String
            -> [FilePath]
            -> [Option]
            -> Wrapper.Config
            -> IO())            -- ^ callback for ok action
        -> Wrapper.Ctx()
showCommitGUI setUpTreeView okCallback = do
    liftIO $ putStrLn "Starting gui ..."
    liftIO $ initGUI
    gui <- loadCommitGUI setUpTreeView

--    -- create and load builder
--    builder <- liftIO $ builderNew
--    liftIO $ builderAddFromFile builder gladepath

--    -- retrieve gtk objects
--    commitDialog <- liftIO $ builderGetObject builder castToDialog (gtkCommitDialog gtkAccessors)
--    actCommit <- liftIO $ builderGetObject builder castToAction (gtkActCommit gtkAccessors)
--    actCancel <- liftIO $ builderGetObject builder castToAction (gtkActCancel gtkAccessors)
--    bufferCommitMsg <- liftIO $ builderGetObject builder castToTextBuffer (gtkBufferCommitMsg gtkAccessors)
--    listView <- liftIO $ builderGetObject builder castToTreeView (gtkListView gtkAccessors)
--    btUnlockTargets <- liftIO $ builderGetObject builder castToCheckButton (gtkBtUnlockTargets gtkAccessors)

--    listStore <- setUpTreeView listView

    -- connect actions
    liftIO $ registerClose $ commitWin gui
    liftIO $ registerCloseAction (actCancel gui) (commitWin gui)

--    liftIO $ on commitDialog deleteEvent $ liftIO $ quit commitDialog >> return False
--    liftIO $ on actCancel actionActivated $ quit commitDialog >> return ()
    config <- ask
    liftIO $ on (getItem (actCommit gui)) actionActivated $ do
                                        let (store,_) = getItem (treeViewFiles gui)
                                        selectedFiles <- getSelectedFiles store

                                        msg <- getGetter (txtViewMsg gui)
                                        okCallback (fromMaybe "" msg) selectedFiles [] config
                                        return ()
--                                        quit commitDialog

    -- present window and start main loop
--    liftIO $ windowPresent commitDialog
--    liftIO $ mainGUI

    liftIO $ putStrLn "Finished"
    return ()


loadCommitGUI :: (TreeView -> Wrapper.Ctx (ListStore SCFile))   -- ^ function to set listStore model for treeview
             -> Wrapper.Ctx CommitGUI
loadCommitGUI setUpTreeView = do
                gladepath <- liftIO getGladepath
                builder <- liftIO $ openGladeFile gladepath
                win <- liftIO $ getWindowFromGlade builder accessorCommitWin
                treeViewFiles <- VCSGui.Common.Commit.getTreeViewFromGladeCustomStore builder accessorTreeViewFiles setUpTreeView
                actCommit <- liftIO $  getActionFromGlade builder accessorActCommit
                actCancel <- liftIO $  getActionFromGlade builder accessorActCancel
                txtViewMsg <- liftIO $  getTextViewFromGlade builder accessorActTxtViewMsg
                return $ LogGUI win treeViewFiles actCommit actCancel txtViewMsg
----
---- HELPERS
----
getTreeViewFromGladeCustomStore :: Builder
                        -> String
                        -> (TreeView -> Wrapper.Ctx (ListStore SCFile)) -- ^ fn defining how to setup the liststore
                        -> Wrapper.Ctx (TreeViewItem SCFile)
getTreeViewFromGladeCustomStore builder name setupListStore = do
    (_, tView) <- liftIO $ wrapWidget builder castToTreeView name
    store <- setupListStore tView
--    liftIO $ treeViewSetModel tView store
    let getter = getFromListStore (store, tView)
        setter = setToListStore (store, tView)
    return (name, (store, tView), (getter, setter))

---
--- same as gtkhelper, but avoiding exposing it
---
wrapWidget :: GObjectClass objClass =>
     Builder
     -> (GObject -> objClass)
     -> String -> IO (String, objClass)
wrapWidget builder cast name = do
    putStrLn $ " cast " ++ name
    gobj <- builderGetObject builder cast name
    return (name, gobj)

getFromListStore :: (ListStore a, TreeView)
    -> IO (Maybe [a])
getFromListStore (store, _) = do
    list <- listStoreToList store
    if null list
        then return Nothing
        else return $ Just list


setToListStore :: (ListStore a, TreeView)
    -> [a]
    -> IO ()
setToListStore (store, view) newList = do
    listStoreClear store
    mapM_ (listStoreAppend store) newList
    return ()

getSelectedFiles :: ListStore SCFile -> IO [FilePath]
getSelectedFiles listStore = do
            listedFiles <- listStoreToList listStore
            let selectedFiles = map (\scf -> filePath scf )
                                $ filter (\scf -> selected scf) listedFiles
            return (selectedFiles)

--
-- deprecated helpers
--
quit :: Dialog -> IO ()
quit commitDialog  = do
        widgetDestroy commitDialog
        liftIO mainQuit

getTextFromBuffer :: TextBuffer -> IO String
getTextFromBuffer buffer = do
        (start, end) <- textBufferGetBounds buffer
        textBufferGetText buffer start end False



-- TODO move this methods to helper?

closeWin :: WindowItem -> IO ()
closeWin win = (widgetHideAll (getItem win))

registerClose :: WindowItem -> IO ()
registerClose win = on (getItem win) deleteEvent (liftIO (closeWin win) >> return False) >> return ()

registerCloseAction :: ActionItem -> WindowItem -> IO ()
registerCloseAction act win = on (getItem act) actionActivated (liftIO (closeWin win)) >> return ()

