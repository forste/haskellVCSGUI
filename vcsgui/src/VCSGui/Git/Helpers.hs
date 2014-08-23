{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Git.Helpers
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Misc helper functions.
--
-----------------------------------------------------------------------------

module VCSGui.Git.Helpers (
    askPassWrapper
) where

import VCSWrapper.Git
import Control.Monad.Reader.Class (asks, MonadReader(..))
import System.Environment (getEnvironment)
import Control.Monad.Reader (liftIO)
import Control.Applicative ((<$>))
import qualified Data.Text as T (pack)


{- | Adds a wrapper to the 'Ctx' so git can ask for a password using a GUI window.
    This is acomplished by setting the GIT_ASKPASS environment variable.
    This is only tested on linux and may not work on MS Windows.
-}
askPassWrapper :: Ctx () -> Ctx ()
askPassWrapper fn = do
    cfgEnv <- asks configEnvironment
    inheritEnv <- map packPair <$> liftIO getEnvironment
    -- TODO better solution for DISPLAY? TODO will this work on windows?
    local (\cfg -> cfg {configEnvironment = ("GIT_ASKPASS", "vcsgui-askpass"):("DISPLAY", ":0.0"):inheritEnv ++ cfgEnv}) fn
  where
    packPair (a, b) = (T.pack a, T.pack b)
