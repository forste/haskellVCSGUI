-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Pull
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Every function related to pulling changes from another Git repository is found in this module.
--
-----------------------------------------------------------------------------

module VCSGui.Git.Pull (
    pull
) where

import Control.Monad.Trans(liftIO)

import qualified VCSWrapper.Git as Git
import qualified VCSWrapper.Common as Wrapper

import VCSGui.Common.Error


-- | Call 'Git.pull'. If the pull fails or a merge conflict is detected an error message is shown.
pull :: Git.Ctx ()
pull = do
    o <- Git.pull
    case o of
        Right ()     -> return ()
        Left msg    -> liftIO $ showErrorGUI $ "MERGE CONFLICT " ++ msg
