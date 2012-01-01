-----------------------------------------------------------------------------
--
-- Module      :  VCSGui.Mercurial.Log
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

module VCSGui.Mercurial.Log (
    showLogGUI
) where

import qualified Graphics.UI.Gtk as Gtk
import qualified VCSGui.Common.Log as Common
import qualified VCSWrapper.Mercurial as Mercurial
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)

{- | Calls 'Common.showLogGUI' using hg. This will display all log entries. The branch to be displayed can be selected.
    Any commit can be checked out, creating a new branch if the commit is not already the HEAD of any branch.
    -}
showLogGUI :: Mercurial.Ctx ()
showLogGUI = do
        log <- Mercurial.simpleLog Nothing
        liftIO $ putStrLn $ "Received log"++show log
        Common.showLogGUI log [] Nothing checkout True
    where
    checkout logEntry Nothing = Mercurial.update (Just $ Mercurial.commitID logEntry)
