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
{-# LANGUAGE DeriveDataTypeable #-}
module VCSGui.Common.MergeTool (
    MergeTool (..)
    , MergeToolSetter

) where


-- | Representation of a mergetool, e.g. kdiff3
data MergeTool = MergeTool {
    fullPath :: FilePath
    } deriving (Show, Read)

-- | Fn to set a 'MergeTool'.
type MergeToolSetter = MergeTool -> IO()


