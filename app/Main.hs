module Main where

import CliOptions (CliOptions(..), parseCliOptions)
import Repl (runRepl)
import qualified Paths_lambda_calculator as P

import Data.Version
import RIO

main :: IO ()
main = runSimpleApp $ do
  CliOptions{..} <- liftIO parseCliOptions

  if version
    then
      logInfo $ "Lambda Calculator (" <> version' <> ")"
    else
      liftIO $ runRepl language
    
-- | Get the current version
version' :: Utf8Builder
version' = fromString $ showVersion P.version
