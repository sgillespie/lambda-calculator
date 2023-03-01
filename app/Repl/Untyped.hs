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
  where replOpts = mkReplOpts banner' (evalLambda . pack) helpMsg
        initialState = mkEvalState defaultUniques

banner' :: MultiLine -> Repl String
banner' _ = unpack <$> prompt (singleton lambda)

evalLambda :: Text -> Repl ()
evalLambda input = do
  state' <- get
  
  let res = runEval (evalText input) state'
  case res of
    Left err -> liftIO . putStrLn . textDisplay $ err
    Right (res', newState) -> do
      put newState
      liftIO . putStrLn . prettyPrint $ res'

helpMsg :: Text
helpMsg = " Commands available: \n"
  <> "    <expression> Evaluate <expression>\n"
  <> "    :help, :h    Show this help\n"
  <> "    :quit, :q    Quit\n\n"
  
  <> " Expressions can take the form:\n"
  <> "    x          variable\n"
  <> "    \\x. t      abstraction\n"
  <> "    f x        function application\n"
  <> "    let x = t  global binding\n\n"

  <> " Examples of valid expressions:\n"
  <> "    x\n"
  <> "    \\x. x\n"
  <> "    (\\x. x) n\n"
  <> "    (\\n f x. f (n f x)) (\\f x. f (f x))\n"
