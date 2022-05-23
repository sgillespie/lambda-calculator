module Repl (runRepl) where

import CliOptions (Language(..))
import Language.Lambda.Util.PrettyPrint
import Paths_lambda_calculator (version)
import qualified Language.Lambda as Lambda
import qualified Language.SystemF as SystemF

import Data.Text (singleton)
import Data.Text.IO (putStrLn)
import Data.Version (showVersion)
import RIO
import RIO.State
import RIO.Text (unpack)
import System.Console.Repline
import qualified Data.Map as M
import qualified RIO.Text.Lazy as Text

type Repl a = HaskelineT (StateT (Lambda.EvalState String) IO) a

newtype AppException
  = ParseError Text

instance Exception AppException

instance Show AppException where
  show = show . toText

toText :: AppException -> Text
toText (ParseError err) = "parse error: " <> err

parseError :: Show show => show -> AppException
parseError = ParseError . fromString . show

runRepl :: Language -> IO ()
runRepl language = evalStateT (evalReplOpts replOpts) initialState
  where replOpts = ReplOpts
          { banner = const $ unpack <$> prompt language,
            command = evalInput language,
            options = commands,
            prefix = Just ':',
            multilineCommand = Nothing,
            tabComplete = Custom completer,
            initialiser = initializer,
            finaliser = return Exit
          }

        initialState = Lambda.mkEvalState Lambda.uniques

prompt :: Language -> Repl Text
prompt language = pure $ prefix language <> " > "
  where prefix Untyped = singleton lambda
        prefix SystemF = singleton upperLambda

evalInput :: Language -> String -> Repl ()
evalInput Untyped input = evalLambda input
evalInput SystemF input = evalSystemF input

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

evalLambda :: String -> Repl ()
evalLambda input = do
  state' <- get
  
  let (res, resState) = runState (Lambda.evalString input) state'
  put resState

  case res of
    Left err -> liftIO . putStrLn . toText . parseError $ err
    Right res' -> liftIO . putStrLn . Text.toStrict . Lambda.prettyPrint $ res'

evalSystemF :: String -> Repl ()
evalSystemF input = case SystemF.evalString M.empty input of
  Left err -> liftIO . putStrLn . toText . parseError $ err
  Right (res, _) -> liftIO . putStrLn . fromString . prettyPrint $ res

helpCommand :: Repl ()
helpCommand = liftIO $ putStrLn banner
  where banner = " Commands available: \n\n"
          <> "    :help, :h\tShow this help\n"
          <> "    :quit, :q\tQuit\n"

version' :: Text
version' = fromString $ showVersion version
