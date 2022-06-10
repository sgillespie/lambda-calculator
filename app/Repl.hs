module Repl (runRepl) where

import CliOptions (Language(..))
import Repl.SystemF (runSystemFRepl)
import Repl.Untyped (runUntypedRepl)

import RIO

runRepl :: Language -> IO ()
runRepl SystemF = runSystemFRepl
runRepl Untyped = runUntypedRepl
