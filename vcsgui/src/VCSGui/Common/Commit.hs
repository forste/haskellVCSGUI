-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- TODO select all files checkbox + TODOs below
--
-- | Functions to show a commit window. This mostly hides the window-building tasks from the specific VCS implementation.
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
import qualified VCSGui.Common.GtkHelper as H
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Paths_vcsgui(getDataFileName)

--
-- glade path and object accessors
--

getGladepath = getDataFileName "data/guiCommonCommit.glade"
accessorWindowCommit = "windowCommit"
accessorTreeViewFiles = "treeViewFiles"
accessorActCommit = "actCommit"
accessorActCancel = "actCancel"
accessorActTxtViewMsg = "txtViewMsg"

--
-- types
--

-- | This function will be called after the ok action is called.
type OkCallBack = String    -- ^ Commit message as specified in the GUI.
            -> [FilePath]   -- ^ List of 'FilePath's of the files that were selected.
            -> [Option]     -- ^ options (this is currently not implemented i.e. '[]' is passed)
            -> Wrapper.Ctx ()

-- | fn to set listStore model for treeview
type TreeViewSetter = TreeView
                   -> Wrapper.Ctx (ListStore SCFile)


data CommitGUI = CommitGUI {
    windowCommit :: H.WindowItem
    , treeViewFiles :: H.TreeViewItem SCFile
    , actCommit :: H.ActionItem
    , actCancel :: H.ActionItem
    , txtViewMsg :: H.TextViewItem
}

-- | Represents a file which can be selected for commiting.
data SCFile = GITSCFile Bool FilePath String |
              SVNSCFile Bool FilePath String Bool
    deriving (Show)

-- | Return 'True' if the 'SCFile' is flagged as selected.
selected :: SCFile -> Bool
selected (GITSCFile s _ _) = s
selected (SVNSCFile s _ _ _) = s

-- | Return the 'FilePath' of this file.
filePath :: SCFile -> FilePath
filePath (GITSCFile _ fp _ ) = fp
filePath (SVNSCFile _ fp _ _) = fp

-- | Return the status of this file.
status :: SCFile -> String
status (GITSCFile _ _ s) = s
status (SVNSCFile _ _ s _) = s

-- | Return 'True' if this file is locked. For Git, this returns always 'False'.
isLocked :: SCFile -> Bool
isLocked (SVNSCFile _ _ _ l) = l
isLocked _                   = False


-- | Options to the 'OkCallBack'.
type Option = String


-- | Display a window to enter a commit message and select files to be commited.
showCommitGUI :: TreeViewSetter
        -> OkCallBack
        -> Wrapper.Ctx()
showCommitGUI setUpTreeView okCallback = do
    liftIO $ putStrLn "Starting gui ..."
    gui <- loadCommitGUI setUpTreeView

    -- connect actions
    liftIO $ H.registerClose $ windowCommit gui
    liftIO $ H.registerCloseAction (actCancel gui) (windowCommit gui)
    config <- ask
    liftIO $ on (H.getItem (actCommit gui)) actionActivated $ do
                                        let (store,_) = H.getItem (treeViewFiles gui)
                                        selectedFiles <- getSelectedFiles store
                                        mbMsg <- H.get (txtViewMsg gui)
                                        case selectedFiles of
                                            [] -> return() -- TODO err-message, selected files are empty
                                            _  -> do
                                                    case mbMsg of
                                                        Nothing -> return() --TODO err-message, message is empty
                                                        Just msg -> Wrapper.runVcs config $ okCallback msg selectedFiles [] -- TODO implement Options
                                        H.closeWin (windowCommit gui)

    -- present window
    liftIO $ widgetShowAll $ H.getItem $ windowCommit gui

    return ()



loadCommitGUI :: TreeViewSetter   -- ^ fn to set listStore model for treeview
                -> Wrapper.Ctx CommitGUI
loadCommitGUI setUpTreeView = do
                gladepath <- liftIO getGladepath
                builder <- liftIO $ H.openGladeFile gladepath

                win <- liftIO $ H.getWindowFromGlade builder accessorWindowCommit
                treeViewFiles <- getTreeViewFromGladeCustomStore builder accessorTreeViewFiles setUpTreeView
                actCommit <- liftIO $  H.getActionFromGlade builder accessorActCommit
                actCancel <- liftIO $  H.getActionFromGlade builder accessorActCancel
                txtViewMsg <- liftIO $  H.getTextViewFromGlade builder accessorActTxtViewMsg
                return $ CommitGUI win treeViewFiles actCommit actCancel txtViewMsg
----
---- HELPERS
----

getSelectedFiles :: ListStore SCFile -> IO [FilePath]
getSelectedFiles listStore = do
            listedFiles <- listStoreToList listStore
            let selectedFiles = map (\scf -> filePath scf )
                                $ filter (\scf -> selected scf) listedFiles
            return (selectedFiles)

getTreeViewFromGladeCustomStore :: Builder
                        -> String
                        -> TreeViewSetter
                        -> Wrapper.Ctx (H.TreeViewItem SCFile)
getTreeViewFromGladeCustomStore builder name setupListStore = do
    (_, tView) <- liftIO $ wrapWidget builder castToTreeView name
    store <- setupListStore tView
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



