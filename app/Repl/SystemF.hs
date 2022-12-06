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
  where replOpts = mkReplOpts banner' $ evalSystemF . pack
        initialState = mkTypecheckState defaultUniques

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
