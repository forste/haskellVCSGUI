-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.Update
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

module VCSGui.Svn.Update (
    showUpdateGUI
) where

import VCSGui.Svn.AskPassword

import qualified VCSWrapper.Svn as Svn
import qualified VCSWrapper.Common as WC

showUpdateGUI :: Either Handler (Maybe String)
          -> WC.Ctx()
showUpdateGUI (Left handler) = do
                                showAskpassGUI (ownHandler handler)
                                return ()
showUpdateGUI (Right mbPwd) = do
                                doUpdate mbPwd
                                return()

ownHandler :: Handler
          -> Handler
ownHandler handler = \result -> do
                                    case result of
                                        Nothing       -> handler result
                                        Just (_,mbPw) -> do
                                                        doUpdate mbPw
                                                        handler result
doUpdate :: Maybe String
         -> WC.Ctx()
doUpdate mbPw = Svn.update mbPw []
