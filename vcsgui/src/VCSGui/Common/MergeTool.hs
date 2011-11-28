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


