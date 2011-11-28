-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Svn.Update
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Svn.Update (
    showUpdateGUI
) where

import qualified VCSGui.Svn.AskPassword as AskPassword
import qualified VCSGui.Svn.Helper as SvnH
import qualified VCSGui.Common.Error as Error
import qualified VCSGui.Common.MergeTool as M
import qualified VCSGui.Common.FilesInConflict as FiC

import qualified VCSWrapper.Svn as Svn
import qualified VCSWrapper.Common as Wrapper

import Control.Monad.Trans(liftIO)

showUpdateGUI :: Either M.MergeTool M.MergeToolSetter -- ^ either a mergetool or a setter for it
          -> Either AskPassword.Handler (Maybe String) -- maybe a password or a handler
          -> Wrapper.Ctx()
showUpdateGUI eMergeToolSetter (Left handler) = do
                                AskPassword.showAskpassGUI $ ownHandler eMergeToolSetter handler
                                return ()
showUpdateGUI eMergeToolSetter (Right mbPwd) = do
                                Svn.update mbPwd []
                                conflictingFiles <- SvnH.getConflictingFiles
                                case conflictingFiles of
                                    [] -> return ()
                                    _ -> FiC.showFilesInConflictGUI
                                                        Nothing
                                                        (conflictingFiles)
                                                        (Svn.getFilesInConflict)
                                                        (\fileToResolve -> Svn.resolved [fileToResolve] Nothing [])
                                                        eMergeToolSetter
                                                        $ (return ())

ownHandler :: Either M.MergeTool M.MergeToolSetter -- ^ either a mergetool or a setter for it
          -> (AskPassword.Handler
          -> AskPassword.Handler)
ownHandler eMergeToolSetter handler = \result -> do
                                    case result of
                                        Nothing       -> handler result
                                        Just (_,mbPw) -> do
                                                        showUpdateGUI eMergeToolSetter $ Right mbPw
                                                        handler result
