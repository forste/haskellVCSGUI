-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.MergeTool
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
{-# LANGUAGE DeriveDataTypeable #-}
module VCSGui.Common.MergeTool (
    exec

) where

import System.Process
import System.Exit
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)
import Control.Concurrent
import Control.Monad.Reader
import qualified Control.Exception as Exc
import Data.Typeable (Typeable)

data MergeToolExceptions = MergeToolExceptions Int String String [String]
    deriving (Show, Typeable)


-- | Internal function to execute a vcs command
exec :: String -- ^ mergetool command, e.g. kdiff3.sh
     -> [String] -- ^ files, last one is output
     -> IO Bool
exec cmd opts = do
    (ec, out, err) <- readProc cmd opts
    case ec of
        ExitSuccess   -> return $ True
        ExitFailure i -> return $ False -- Left $ MergeToolExceptions i out err (cmd : opts)

 -- same as readProcessWithExitCode but having a configurable cwd and env,
readProc :: String  --command
            -> [String] -- ^ files, last one is output
            -> IO (ExitCode, String, String)
readProc cmd files = do
    putStrLn $ "Executing process, cmd: "++show cmd++",files: "++show files
    (_, Just outh, Just errh, pid) <- createProcess (proc cmd files)
                                            { std_in = CreatePipe,
                                              std_out = CreatePipe,
                                              std_err = CreatePipe
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
    return (ex, out, err)

---- just exec with stdin/stdout/stderr as pipes
--execProcWithPipes :: Maybe FilePath
--                    -> String
--                    -> [String]
--                     -> [(String, String)]
--                  -> IO (Handle, Handle, Handle, ProcessHandle)
--execProcWithPipes mcwd command args menv = do
--    (Just inh, Just outh, Just errh, pid) <- createProcess (proc command args)
--        { std_in = CreatePipe,
--          std_out = CreatePipe,
--          std_err = CreatePipe,
--          cwd = mcwd,
--          env = Just menv }
--    return (inh, outh, errh, pid)
