module Repl.Shared where

import CliOptions (Language(..))
import Paths_lambda_calculator (version)
import Language.Lambda.Shared.Errors (LambdaException())
import Language.Lambda.Shared.UniqueSupply (defaultUniques)
import Language.Lambda.SystemF

import Data.Text (singleton)
import Data.Text.IO (putStrLn)
import Data.Version (showVersion)
import RIO
import RIO.State
import RIO.Text (pack, unpack)
import System.Console.Repline
import Control.Monad.Except
import qualified Data.Map as M

mkReplOpts banner command = ReplOpts
  { banner = banner,
    command = command,
    options = commands,
    prefix = Just ':',
    multilineCommand = Nothing,
    tabComplete = Custom completer,
    initialiser = initializer,
    finaliser = return Exit
  }

prompt :: Applicative ap => Text -> HaskelineT ap Text
prompt prefix = pure $ prefix <> " > "

commands :: (MonadIO m, MonadThrow m) => [(String, String -> HaskelineT m ())]
commands
  = [ ("h", help'),
      ("help", help'),
      ("q", quit'),
      ("quit", quit')
    ]
  where help' = const helpCommand
        quit' = const abort

completer :: Monad m => CompletionFunc m
completer (left, _) = pure (left, []) -- No tab completion

initializer :: MonadIO io => HaskelineT io ()
initializer = liftIO $ putStrLn greeting
  where greeting = "Lambda Calculator ("
          <> version'
          <> ")\nType :h for help\n"

helpCommand :: MonadIO io => HaskelineT io ()
helpCommand = liftIO $ putStrLn banner
  where banner = " Commands available: \n\n"
          <> "    :help, :h\tShow this help\n"
          <> "    :quit, :q\tQuit\n"

version' :: Text
version' = fromString $ showVersion version
