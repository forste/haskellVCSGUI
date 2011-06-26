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

askPassWrapper :: Ctx () -> Ctx ()
askPassWrapper fn = do
    env <- asks configEnvironment
    -- TODO better solution for DISPLAY? TODO will this work on windows?
    local (\cfg -> cfg {configEnvironment = ("GIT_ASKPASS", "ssh-askpass"):("DISPLAY", ":0.0"):env}) fn

