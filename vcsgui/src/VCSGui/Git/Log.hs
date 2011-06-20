-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Log
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

module VCSGui.Git.Log (
    showLogGUI
) where

import qualified VCSGui.Common.Log as Common
import qualified VCSWrapper.Git as Git
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)

-- TODO handle new branch creation
showLogGUI :: Git.Ctx ()
showLogGUI = do
        log <- Git.simpleLog Nothing
        branches <- Git.localBranches
        Common.showLogGUI log [] (Just (branches, \branch -> Git.simpleLog (Just branch))) checkout
    where
    checkout log Nothing = Git.checkout (Just $ Git.commitID log) Nothing
    checkout log (Just selBranch) = do
        revBranch <- Git.revparse selBranch
        liftIO $ putStrLn $ "revBranch: " ++ revBranch ++ " selBranch: " ++ selBranch
        liftIO $ putStrLn $ "commitID:  " ++ (Git.commitID log)
        case ((Git.commitID log) == revBranch) of
            True -> do
                liftIO $ putStrLn "checking out selected Branch"
                Git.checkout (Just selBranch) Nothing
            False -> do
                liftIO $ putStrLn $ "checking out Commit " ++ (Git.commitID log)
                Git.checkout (Just $ Git.commitID log) Nothing



