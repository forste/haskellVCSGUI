-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Pull
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

module VCSGui.Git.Pull (
    pull
) where

import Control.Monad.Trans(liftIO)
import Control.Monad.Reader.Class (ask)

import qualified VCSWrapper.Git as Git
import qualified VCSWrapper.Common as Wrapper

import VCSGui.Common.Error
import qualified VCSGui.Common.FilesInConflict as FiC

pull :: Git.Ctx ()
pull = do
    o <- Git.pull
    case o of
        Right ()    -> return ()
--        Left msg    -> liftIO $ showErrorGUI $ "MERGE CONFLICT " ++ msg
        Left msg    -> do
            config <- ask
            status <- Git.status
            let conflictingFiles = [ Git.filePath s | s <- status, (Git.modification s) == Git.Conflicting ]
            FiC.showFilesInConflictGUIinternalMergeTool
                            Nothing
                            conflictingFiles
                            (\fp -> Git.mergetool fp >> return True)
                            (\f -> Git.add [f])
                            (do
                                o <- Git.commitMerge
                                case o of
                                    Right () -> return ()
                                    Left msg -> liftIO $ putStrLn ("merge commit failed " ++ msg) >> return () -- TODO ask user if merge was successfull if git mergetool expects it
                                    )

