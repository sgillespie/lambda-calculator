module Repl.Shared where

import Paths_lambda_calculator (version)

import Data.Text.IO (putStrLn)
import Data.Version (showVersion)
import RIO
import System.Console.Repline

mkReplOpts
  :: (MonadIO m, MonadThrow m)
  => (MultiLine -> HaskelineT m String)
  -> Command (HaskelineT m)
  -> Text
  -> ReplOpts m
mkReplOpts banner command helpMsg = ReplOpts
  { banner = banner,
    command = command,
    options = commands helpMsg,
    prefix = Just ':',
    multilineCommand = Nothing,
    tabComplete = Custom completer,
    initialiser = initializer,
    finaliser = return Exit
  }

prompt :: Applicative ap => Text -> HaskelineT ap Text
prompt prefix = pure $ prefix <> " > "

commands :: (MonadIO m, MonadThrow m) => Text -> [(String, String -> HaskelineT m ())]
commands helpMsg
  = [ ("h", help'),
      ("help", help'),
      ("q", quit'),
      ("quit", quit')
    ]
  where help' = const (helpCommand helpMsg)
        quit' = const abort

completer :: Monad m => CompletionFunc m
completer (left, _) = pure (left, []) -- No tab completion

initializer :: MonadIO io => HaskelineT io ()
initializer = liftIO $ putStrLn greeting
  where greeting = "Lambda Calculator ("
          <> version'
          <> ")\nType :h for help\n"

helpCommand :: MonadIO io => Text -> HaskelineT io ()
helpCommand message = liftIO $ putStrLn message

version' :: Text
version' = fromString $ showVersion version
