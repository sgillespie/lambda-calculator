module Repl (runRepl) where

import CliOptions (Language(..))
import Paths_lambda_calculator (version)
import Language.Lambda.Untyped.State
import Language.Lambda.Shared.Errors (LambdaException())
import qualified Language.Lambda.Untyped as Untyped
import qualified Language.Lambda.SystemF as SystemF

import Data.Text (singleton)
import Data.Text.IO (putStrLn)
import Data.Version (showVersion)
import RIO
import RIO.State
import RIO.Text (pack, unpack)
import System.Console.Repline
import qualified Data.Map as M
import Control.Monad.Except

type EvalT name m
  = StateT (Untyped.EvalState name)
      (ExceptT LambdaException m)

type Repl a = HaskelineT (EvalT Text IO) a

runRepl :: Language -> IO ()
runRepl language
  = void . runExceptT . evalStateT (evalReplOpts replOpts) $ initialState
  where replOpts = ReplOpts
          { banner = const $ unpack <$> prompt language,
            command = flip evalInput language,
            options = commands,
            prefix = Just ':',
            multilineCommand = Nothing,
            tabComplete = Custom completer,
            initialiser = initializer,
            finaliser = return Exit
          }

        initialState = Untyped.mkEvalState Untyped.defaultUniques

prompt :: Language -> Repl Text
prompt language = pure $ prefix language <> " > "
  where prefix Untyped = singleton Untyped.lambda
        prefix SystemF = singleton SystemF.upperLambda

evalInput :: String -> Language -> Repl ()
evalInput input = \case
  Untyped -> evalLambda input'
  SystemF -> evalSystemF input'

  where input' = pack input

commands :: [(String, String -> Repl ())]
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

initializer :: Repl ()
initializer = liftIO $ putStrLn greeting
  where greeting = "Lambda Calculator ("
          <> version'
          <> ")\nType :h for help\n"

evalLambda :: Text -> Repl ()
evalLambda input = do
  state' <- get
  
  let res = runEval (Untyped.evalText input) state'
  case res of
    Left err -> liftIO . putStrLn . textDisplay $ err
    Right (res', newState) -> do
      put newState
      liftIO . putStrLn . Untyped.prettyPrint $ res'

evalSystemF :: Text -> Repl ()
evalSystemF input = case SystemF.evalText M.empty input of
  Left err -> liftIO . putStrLn . pack . show $ err
  Right (res, _) -> liftIO . putStrLn . SystemF.prettyPrint $ res

helpCommand :: Repl ()
helpCommand = liftIO $ putStrLn banner
  where banner = " Commands available: \n\n"
          <> "    :help, :h\tShow this help\n"
          <> "    :quit, :q\tQuit\n"

version' :: Text
version' = fromString $ showVersion version
