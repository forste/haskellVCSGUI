{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.Process
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides facilites to execute an external program.
--
-----------------------------------------------------------------------------

module VCSGui.Common.Process (
    exec
) where

import System.Process
import System.Exit
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)
import Control.Concurrent
import Control.Monad.Reader
import qualified Control.Exception as Exc
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)

-- | Internal function to execute a vcs command
exec :: Maybe FilePath -- ^ working directory or Nothing if not set
     -> Text -- ^ mergetool command, e.g. kdiff3.sh
     -> [Text] -- ^ files, last one is output
     -> IO Bool
exec mcwd cmd opts = do
    (ec, out, err) <- readProc mcwd cmd opts
    case ec of
        ExitSuccess   -> return $ True
        ExitFailure i -> return $ False -- Left $ Exception i out err (cmd : opts)

-- | same as readProcessWithExitCode but having a configurable cwd and env,
readProc :: Maybe FilePath --working directory or Nothing if not set
            -> Text  --command
            -> [Text] -- ^ files, last one is output
            -> IO (ExitCode, Text, Text)
readProc mcwd cmd files = do
    putStrLn $ "Executing process, mcwd: "++show mcwd++"cmd: "++show cmd++",files: "++show files
    (_, Just outh, Just errh, pid) <- createProcess (proc (T.unpack cmd) (map T.unpack files))
                                            { std_out = CreatePipe,
                                              std_err = CreatePipe,
                                              cwd = mcwd
                                              }

    outMVar <- newEmptyMVar

    out <- hGetContents outh
    _ <- forkIO $ Exc.evaluate (length out) >> putMVar outMVar ()

    err <- hGetContents errh
    _ <- forkIO $ Exc.evaluate (length err) >> putMVar outMVar ()

--    hClose inh

    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    ex <- waitForProcess pid
    return (ex, T.pack out, T.pack err)


