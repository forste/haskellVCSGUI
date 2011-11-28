-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.ExceptionHandler
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

module VCSGui.Common.ExceptionHandler (
    defaultVCSExceptionHandler
) where

import qualified Control.Exception as Exc

import VCSWrapper.Common
import VCSGui.Common.Error

defaultVCSExceptionHandler :: IO t -> IO ()
defaultVCSExceptionHandler vcsRunner = do
    o <- Exc.try vcsRunner
    putStrLn $ "exception handler called"
    case o of
        Left (VCSException exitCode out err repoLocation (cmd:opts)) -> do
            putStrLn $ "exception caught"
            showErrorGUI $ unlines ["An error occured.", err, "Details:", "command: " ++ cmd, "options: " ++ unwords opts]
        Right _ -> do
            putStrLn $ "no exception"
            return ()
