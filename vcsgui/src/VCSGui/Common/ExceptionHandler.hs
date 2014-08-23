{-# LANGUAGE OverloadedStrings #-}
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
-- | Exception handling functions can be found in this module.
--
-----------------------------------------------------------------------------

module VCSGui.Common.ExceptionHandler (
    defaultVCSExceptionHandler
) where

import qualified Control.Exception as Exc

import VCSWrapper.Common
import VCSGui.Common.Error
import qualified Data.Text as T (unwords, unlines)
import Data.Monoid ((<>))


-- | Wraps an IO computation to display an error message if a 'VCSException' occurs.
defaultVCSExceptionHandler :: IO t -> IO ()
defaultVCSExceptionHandler vcsRunner = do
    o <- Exc.try vcsRunner
    putStrLn $ "exception handler called"
    case o of
        Left (VCSException exitCode out err repoLocation (cmd:opts)) -> do
            putStrLn $ "exception caught"
            showErrorGUI $ T.unlines ["An error occured.", err, "Details:", "command: " <> cmd, "options: " <> T.unwords opts]
        Right _ -> do
            putStrLn $ "no exception"
            return ()
