-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Helpers
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

module VCSGui.Git.Helpers (
    askPassWrapper
) where

import VCSWrapper.Git
import Control.Monad.Reader.Class (asks, MonadReader(..))
import System.Environment (getEnvironment)
import Control.Monad.Reader (liftIO)

askPassWrapper :: Ctx () -> Ctx ()
askPassWrapper fn = do
    cfgEnv <- asks configEnvironment
    inheritEnv <- liftIO $ getEnvironment
    -- TODO better solution for DISPLAY? TODO will this work on windows?
    local (\cfg -> cfg {configEnvironment = ("GIT_ASKPASS", "vcsgui-askpass"):("DISPLAY", ":0.0"):inheritEnv ++ cfgEnv}) fn

