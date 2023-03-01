module Repl.SystemF (runSystemFRepl) where

import Language.Lambda.Shared.Errors (LambdaException())
import Language.Lambda.Shared.UniqueSupply (defaultUniques)
import Language.Lambda.SystemF
import Repl.Shared

import Data.Text (singleton)
import Data.Text.IO (putStrLn)
import RIO
import RIO.State
import RIO.Text (pack, unpack)
import System.Console.Repline
import Control.Monad.Except (ExceptT(..), runExceptT)

type EvalT name m
  = StateT (TypecheckState name)
      (ExceptT LambdaException m)

type Repl a = HaskelineT (EvalT Text IO) a

runSystemFRepl :: IO ()
runSystemFRepl
  = void . runExceptT . evalStateT (evalReplOpts replOpts) $ initialState
  where replOpts = mkReplOpts banner' (evalSystemF . pack) helpMsg
        initialState = mkTypecheckState defaultUniques defaultTyUniques

banner' :: MultiLine -> Repl String
banner' _ = unpack <$> prompt (singleton upperLambda)

evalSystemF :: Text -> Repl ()
evalSystemF input = do
  state' <- get

  let res = runTypecheck (evalText input) state'
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
  <> "    x:T        type annotated variable\n"
  <> "    \\x:T. t    abstraction\n"
  <> "    f x        function application\n"
  <> "    \\X. t      type abstraction\n"
  <> "    x [T]      type application\n"
  <> "    let x = t  global binding\n\n"

  <> " Types can take the form:\n"
  <> "    T            type variable\n"
  <> "    T -> T       type of functions\n"
  <> "    forall X. T  universal type\n\n"

  <> " Examples of valid expressions:\n"
  <> "    x\n"
  <> "    \\x:T. x\n"
  <> "    (\\x:T. x) y:T\n"
  <> "    \\X. \\x:X. x\n"
  <> "    \\x:(forall T. T). x\n"
  <> "    (\\n:((T->T)->T->T) f:(T->T) x:T. f (n f x)) (\\f:(T->T) x:T. x)\n"
