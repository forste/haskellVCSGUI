-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.MergeTool
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Types associated with resolving conflicts with a 'Mergetool'.
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module VCSGui.Common.MergeTool (
    MergeTool (..)
    , MergeToolSetter

) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | Representation of a mergetool, e.g. kdiff3
newtype MergeTool = MergeTool {
    fullPath :: FilePath
    } deriving (Show, Read, Generic)

instance ToJSON MergeTool
instance FromJSON MergeTool

-- | Fn to set a 'MergeTool'.
type MergeToolSetter = MergeTool -> IO()


