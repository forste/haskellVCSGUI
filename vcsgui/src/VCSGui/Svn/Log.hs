-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.Log
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides a GUI to show log for current working copy.
--
-----------------------------------------------------------------------------

module VCSGui.Svn.Log (
    showLogGUI
) where

import qualified VCSGui.Common.Log as C
import VCSGui.Svn.AskPassword

import qualified VCSWrapper.Svn as Svn
import qualified VCSWrapper.Common as WC

-- | Shows a GUI showing log for current working copy.
showLogGUI :: Either Handler (Maybe String) -- ^ Either 'Handler' for password request or password (nothing for no password)
           -> Svn.Ctx ()
showLogGUI eitherHandlerOrPw = do
        logEntries <- Svn.simpleLog
        C.showLogGUI logEntries [] Nothing $ okCallback eitherHandlerOrPw

okCallback :: Either Handler (Maybe String) -- ^ either callback for password request or password (nothing for no password)
           -> WC.LogEntry                   -- ^ chosen logentry
           -> Maybe String                  -- ^ chosen branch name
           -> WC.Ctx()
okCallback eitherHandlerOrPw logEntry _ = do
    case eitherHandlerOrPw of
                                    Left handler -> do
                                                        showAskpassGUI (ownHandler handler)
                                                        return ()
                                    Right pw     -> doRevert logEntry pw


    where
    ownHandler :: Handler
               -> Handler
    ownHandler handler = \result -> do
                                        case result of
                                            Nothing       -> handler result
                                            Just (_,mbPw) -> do
                                                            doRevert logEntry mbPw
                                                            handler result
    doRevert logEntry mbPw = do
                            Svn.mergeHeadToRevision (revision logEntry) mbPw []
                            return()
    revision logEntry = read $ Svn.commitID logEntry :: Integer


