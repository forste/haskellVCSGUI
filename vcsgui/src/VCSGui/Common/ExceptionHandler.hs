-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.ExceptionHandler
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

module VCSGui.Common.ExceptionHandler (
    defaultVCSExceptionHandler
) where

import qualified Control.Exception as Exc

import VCSWrapper.Common
import VCSGui.Common.Error

defaultVCSExceptionHandler :: IO t -> IO ()
defaultVCSExceptionHandler vcsRunner = do
    o <- Exc.try vcsRunner
    case o of
        Left (VCSException exitCode out err repoLocation (cmd:opts)) ->
            showErrorGUI $ unlines ["An error occured.", err, "Details:", "command: " ++ cmd, "options: " ++ unwords opts]
        Right _ -> return ()
