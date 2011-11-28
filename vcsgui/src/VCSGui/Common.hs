-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
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
    , VCSGui.Common.ExceptionHandler.defaultVCSExceptionHandler
    , module VCSGui.Common.MergeTool
) where
import qualified VCSGui.Common.SetupConfig
import qualified VCSGui.Common.Error
import qualified VCSGui.Common.ExceptionHandler
import VCSGui.Common.MergeTool
