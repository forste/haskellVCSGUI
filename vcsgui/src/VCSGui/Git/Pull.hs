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
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Git.Pull (
    pull
) where

import Control.Monad.Trans(liftIO)

import qualified VCSWrapper.Git as Git
import qualified VCSWrapper.Common as Wrapper

import VCSGui.Common.Error

pull :: Git.Ctx ()
pull = do
    o <- Git.pull
    case o of
        Right ()     -> return ()
        Left msg    -> liftIO $ showErrorGUI $ "MERGE CONFLICT " ++ msg
