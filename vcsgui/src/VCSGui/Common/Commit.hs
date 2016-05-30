{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Paths_vcsgui(getDataFileName)
import qualified Data.Text as T (unpack, pack)
import Data.Text (Text)
import GI.Gtk.Objects.TreeView (TreeView(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreAppend, seqStoreClear, seqStoreToList, SeqStore(..))
import GI.Gtk.Objects.Action (onActionActivate)
import GI.Gtk.Objects.Widget (widgetShowAll)
import GI.Gtk.Objects.Builder (builderGetObject, Builder(..))
import Foreign.ForeignPtr (ForeignPtr)
import Data.GI.Base.BasicTypes (NullToNothing(..), GObject)
import Data.GI.Base.ManagedPtr (unsafeCastTo)

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
type OkCallBack = Text    -- ^ Commit message as specified in the GUI.
            -> [FilePath]   -- ^ List of 'FilePath's of the files that were selected.
            -> [Option]     -- ^ options (this is currently not implemented i.e. '[]' is passed)
            -> Wrapper.Ctx ()

-- | fn to set seqStore model for treeview
type TreeViewSetter = TreeView
                   -> Wrapper.Ctx (SeqStore SCFile)


data CommitGUI = CommitGUI {
    windowCommit :: H.WindowItem
    , treeViewFiles :: H.TreeViewItem SCFile
    , actCommit :: H.ActionItem
    , actCancel :: H.ActionItem
    , txtViewMsg :: H.TextViewItem
}

-- | Represents a file which can be selected for commiting.
data SCFile = GITSCFile Bool FilePath Text |
              SVNSCFile Bool FilePath Text Bool
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
status :: SCFile -> Text
status (GITSCFile _ _ s) = s
status (SVNSCFile _ _ s _) = s

-- | Return 'True' if this file is locked. For Git, this returns always 'False'.
isLocked :: SCFile -> Bool
isLocked (SVNSCFile _ _ _ l) = l
isLocked _                   = False


-- | Options to the 'OkCallBack'.
type Option = Text


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
    liftIO $ onActionActivate (H.getItem (actCommit gui)) $ do
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



loadCommitGUI :: TreeViewSetter   -- ^ fn to set seqStore model for treeview
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

getSelectedFiles :: SeqStore SCFile -> IO [FilePath]
getSelectedFiles seqStore = do
            listedFiles <- seqStoreToList seqStore
            let selectedFiles = map (\scf -> filePath scf )
                                $ filter (\scf -> selected scf) listedFiles
            return (selectedFiles)

getTreeViewFromGladeCustomStore :: Builder
                        -> Text
                        -> TreeViewSetter
                        -> Wrapper.Ctx (H.TreeViewItem SCFile)
getTreeViewFromGladeCustomStore builder name setupSeqStore = do
    (_, tView) <- liftIO $ wrapWidget builder TreeView name
    store <- setupSeqStore tView
    let getter = getFromSeqStore (store, tView)
        setter = setToSeqStore (store, tView)
    return (name, (store, tView), (getter, setter))

---
--- same as gtkhelper, but avoiding exposing it
---
wrapWidget :: GObject objClass =>
     Builder
     -> (ForeignPtr objClass -> objClass)
     -> Text -> IO (Text, objClass)
wrapWidget builder constructor name = do
    putStrLn $ " cast " ++ T.unpack name
    gobj <- nullToNothing (builderGetObject builder name) >>= unsafeCastTo constructor . fromJust
    return (name, gobj)

getFromSeqStore :: (SeqStore a, TreeView)
    -> IO (Maybe [a])
getFromSeqStore (store, _) = do
    list <- seqStoreToList store
    if null list
        then return Nothing
        else return $ Just list

setToSeqStore :: (SeqStore a, TreeView)
    -> [a]
    -> IO ()
setToSeqStore (store, view) newList = do
    seqStoreClear store
    mapM_ (seqStoreAppend store) newList
    return ()



