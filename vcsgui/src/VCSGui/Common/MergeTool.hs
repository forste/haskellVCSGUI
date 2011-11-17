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
    MergeTool (..)
    , MergeToolSetter

) where



data MergeTool = MergeTool {
    fullPath :: FilePath
    } deriving (Show, Read)

type MergeToolSetter = MergeTool -> IO()


