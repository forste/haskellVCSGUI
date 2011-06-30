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
-- | TODO select all files checkbox + TODOs below
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
import qualified VCSGui.Common.GtkHelper as H
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Maybe
import Paths_vcsgui(getDataFileName)

--
-- glade path and object accessors
--

getGladepath = getDataFileName "guiCommonCommit.glade"
accessorWindowCommit = "windowCommit"
accessorTreeViewFiles = "treeViewFiles"
accessorActCommit = "actCommit"
accessorActCancel = "actCancel"
accessorActTxtViewMsg = "txtViewMsg"

--
-- types
--

data CommitGUI = LogGUI {
    windowCommit :: H.WindowItem
    , treeViewFiles :: H.TreeViewItem SCFile
    , actCommit :: H.ActionItem
    , actCancel :: H.ActionItem
    , txtViewMsg :: H.TextViewItem
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



showCommitGUI :: (TreeView -> Wrapper.Ctx (ListStore SCFile))   -- ^ fn to set listStore model for treeview
        -> (   String                           -- ^ commit message
            -> [FilePath]                       -- ^ selected files
            -> [Option]                         -- ^ options TODO not implemented
            -> Wrapper.Ctx ())                                  -- ^ callback for ok action
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
                                                        Just msg -> Wrapper.runVcs config $ okCallback msg selectedFiles []
                                        H.closeWin (windowCommit gui)

    -- present window
    liftIO $ widgetShowAll $ H.getItem $ windowCommit gui

    return ()



loadCommitGUI :: (TreeView
                    -> Wrapper.Ctx (ListStore SCFile))   -- ^ fn to set listStore model for treeview
                -> Wrapper.Ctx CommitGUI
loadCommitGUI setUpTreeView = do
                gladepath <- liftIO getGladepath
                builder <- liftIO $ H.openGladeFile gladepath

                win <- liftIO $ H.getWindowFromGlade builder accessorWindowCommit
                treeViewFiles <- getTreeViewFromGladeCustomStore builder accessorTreeViewFiles setUpTreeView
                actCommit <- liftIO $  H.getActionFromGlade builder accessorActCommit
                actCancel <- liftIO $  H.getActionFromGlade builder accessorActCancel
                txtViewMsg <- liftIO $  H.getTextViewFromGlade builder accessorActTxtViewMsg
                return $ LogGUI win treeViewFiles actCommit actCancel txtViewMsg
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
                        -> (TreeView -> Wrapper.Ctx (ListStore SCFile)) -- ^ fn defining how to setup the liststore
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



