-----------------------------------------------------------------------------
--
-- Module      :  Main
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
module VCSGui.Common.Commit (
    showGUI,
    GTKObjectAccessors(..),
    VCSType(..)
) where

import qualified VCSGui.Svn.Commit as Svn
import VCSGui.Common.Types
import VCSGui.Common.Log

-- loads gui objects and connects them
showGUI :: String               -- author
        -> FilePath             -- current working directory
        -> FilePath             -- glade
        -> GTKObjectAccessors   -- accessors for gtk objects
        -> VCSType              -- version control type
        -> IO()
showGUI cwd author gladepath gtkAccessors SVN = return ()
--showGUI cwd author gladepath gtkAccessors SVN = do
--    Svn.showGUI cwd author gladepath gtkAccessors




--data CheckoutCtx a = CheckoutCtx (ReaderT CheckoutConfig IO a)
--   deriving (Monad)
--   , MonadIO b, MonadReader CheckoutConfig b)

--usage e.g. runWithConfig $ loadGui "path/to/repo"
--  where
--      runWithConfig = runCtx curConfig
--      runCtx config (Checkout a) = runReaderT a config
--      curConfig = makeConfig options ...

--loadGui :: FilePath -> CheckoutCtx ()


