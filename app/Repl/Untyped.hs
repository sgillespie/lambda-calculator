module Repl.Untyped (runUntypedRepl) where

import Language.Lambda.Shared.Errors (LambdaException())
import Language.Lambda.Shared.UniqueSupply (defaultUniques)
import Language.Lambda.Untyped
import Repl.Shared

import Data.Text (singleton)
import Data.Text.IO (putStrLn)
import RIO
import RIO.State
import RIO.Text (pack, unpack)
import System.Console.Repline
import Control.Monad.Except

type EvalT name m
  = StateT (EvalState name)
      (ExceptT LambdaException m)

type Repl a = HaskelineT (EvalT Text IO) a

runUntypedRepl :: IO ()
runUntypedRepl
  = void . runExceptT . evalStateT (evalReplOpts replOpts) $ initialState
  where replOpts = ReplOpts
          { banner = const $ unpack <$> prompt',
            command = evalLambda . pack,
            options = commands,
            prefix = Just ':',
            multilineCommand = Nothing,
            tabComplete = Custom completer,
            initialiser = initializer,
            finaliser = return Exit
          }

        initialState = mkEvalState defaultUniques

prompt' :: Repl Text
prompt' = prompt $ singleton lambda

evalLambda :: Text -> Repl ()
evalLambda input = do
  state' <- get
  
  let res = runEval (evalText input) state'
  case res of
    Left err -> liftIO . putStrLn . textDisplay $ err
    Right (res', newState) -> do
      put newState
      liftIO . putStrLn . prettyPrint $ res'
