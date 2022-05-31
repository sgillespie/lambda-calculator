{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda.Untyped (
  evalString,
  runEvalString,
  execEvalString,
  unsafeExecEvalString,
  defaultUniques,

  module Language.Lambda.Untyped.Expression,
  module Language.Lambda.Untyped.Eval,
  module Language.Lambda.Untyped.Parser,
  module Language.Lambda.Untyped.State
  ) where

import Control.Monad.Except
import Data.Either
import RIO
import RIO.Text (pack)

import Language.Lambda.Shared.Errors
import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Expression
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.State

evalString :: String -> Eval String (LambdaExpr String)
evalString = either throwParseError evalExpr . parseExpr
  where throwParseError = throwError . ParseError . pack . show

runEvalString
  :: String
  -> Globals String
  -> Either LambdaException (LambdaExpr String, EvalState String)
runEvalString input globals' = runEval (evalString input) (mkState globals')

execEvalString
  :: String
  -> Globals String
  -> Either LambdaException (LambdaExpr String)
execEvalString input globals' = execEval (evalString input) (mkState globals')

unsafeExecEvalString
  :: String
  -> Globals String
  -> LambdaExpr String
unsafeExecEvalString input globals'
  = unsafeExecEval (evalString input) (mkState globals')

defaultUniques :: [String]
defaultUniques = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
  where suffix = "" : map show [(0::Int)..]

mkState :: Globals String -> EvalState String
mkState = flip EvalState defaultUniques
