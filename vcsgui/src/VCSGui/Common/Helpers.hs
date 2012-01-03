-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Common.Helpers
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL Nothing
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSGui.Common.Helpers (
    emptyListToNothing
) where

-- | Return 'Nothing' if given list is empty, 'Just' the list otherwise.
emptyListToNothing :: [a] -> Maybe [a]
emptyListToNothing [] = Nothing
emptyListToNothing l = Just l
