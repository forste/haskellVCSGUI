-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Common gui elements and functions are declared here.
-- This module is not an exposed library, every vcs-main-module should reexport this module instead
--
-----------------------------------------------------------------------------

module VCSGui.Common (
    VCSGui.Common.Error.showErrorGUI
    , VCSGui.Common.SetupConfig.showSetupConfigGUI
) where
import qualified VCSGui.Common.SetupConfig
import qualified VCSGui.Common.Error

