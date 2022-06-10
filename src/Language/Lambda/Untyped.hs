{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda.Untyped (
  evalText,
  runEvalText,
  execEvalText,
  unsafeExecEvalText,
  defaultUniques,

  module Language.Lambda.Untyped.Expression,
  module Language.Lambda.Untyped.Eval,
  module Language.Lambda.Untyped.Parser,
  module Language.Lambda.Untyped.State
  ) where

import Control.Monad.Except
import Data.Either
import RIO
import qualified RIO.Text as Text

import Language.Lambda.Shared.Errors
import Language.Lambda.Shared.UniqueSupply (defaultUniques)
import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Expression
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.State

evalText :: Text -> Eval Text (LambdaExpr Text)
evalText = either throwParseError evalExpr' . parseExpr
  where throwParseError = throwError . ParseError . Text.pack . show
        evalExpr' = evalExpr

runEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (LambdaExpr Text, EvalState Text)
runEvalText input globals' = runEval (evalText input) (mkState globals')

execEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (LambdaExpr Text)
execEvalText input globals' = execEval (evalText input) (mkState globals')

unsafeExecEvalText
  :: Text
  -> Globals Text
  -> LambdaExpr Text
unsafeExecEvalText input globals'
  = unsafeExecEval (evalText input) (mkState globals')

mkState :: Globals Text -> EvalState Text
mkState = flip EvalState defaultUniques
