-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.Checkout
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

module VCSGui.Svn.Checkout (
    showCheckoutGUI
    ,SVNCheckoutObjectAccessors(..)
) where

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Monad
import Control.Monad.Reader
import qualified VCSWrapper.Svn as Svn

data SVNCheckoutObjectAccessors = SVNCheckoutObjectAccessors {
    gtkCheckoutWindow :: String
    ,gtkActCheckout :: String
    ,gtkActCancel :: String
    ,gtkBufferURL :: String
    ,gtkBufferRevision :: String
    ,gtkBufferPath :: String
    }

showCheckoutGUI :: FilePath             -- ^ glade
        -> SVNCheckoutObjectAccessors   -- ^ accessors for gtk objects
        -> Svn.Ctx()
showCheckoutGUI gladepath gtkAccessors = do
    liftIO $ putStrLn "Starting gui ..."
    liftIO $ initGUI

    -- create and load builder
    builder <- liftIO $ builderNew
    liftIO $ builderAddFromFile builder gladepath

    -- retrieve gtk objects
    checkoutWindow <- liftIO $ builderGetObject builder castToWindow (gtkCheckoutWindow gtkAccessors)
    actCheckout <- liftIO $ builderGetObject builder castToAction (gtkActCheckout gtkAccessors)
    actCancel <- liftIO $ builderGetObject builder castToAction (gtkActCancel gtkAccessors)
    bufferURL <- liftIO $ builderGetObject builder castToTextBuffer (gtkBufferURL gtkAccessors)
    bufferRevision <- liftIO $ builderGetObject builder castToTextBuffer (gtkBufferRevision gtkAccessors)
    bufferPath <- liftIO $ builderGetObject builder castToTextBuffer (gtkBufferPath gtkAccessors)

     -- connect actions
    liftIO $ on checkoutWindow deleteEvent $ liftIO $ quit checkoutWindow >> return False
    liftIO $ on actCancel actionActivated $ quit checkoutWindow >> return ()
    config <- ask
    liftIO $ on actCheckout actionActivated $ do
                                        url <- getTextFromBuffer bufferURL
                                        revision <- getTextFromBuffer bufferRevision
                                        let realRevision = if revision == "" then Nothing else Just revision
                                        filePath <- getTextFromBuffer bufferPath
                                        let realFilePath = if filePath == "" then Nothing else Just filePath
                                        Svn.runVcs config $ Svn.checkout [(url, realRevision)] realFilePath []
                                        quit checkoutWindow
    liftIO $ windowPresent checkoutWindow
    liftIO $ mainGUI

    liftIO $ putStrLn "Finished"
    return ()

----
---- HELPERS
----
quit :: Window -> IO ()
quit commitDialog  = do
        widgetDestroy commitDialog
        liftIO mainQuit

getTextFromBuffer :: TextBuffer -> IO String
getTextFromBuffer buffer = do
        (start, end) <- textBufferGetBounds buffer
        textBufferGetText buffer start end False
