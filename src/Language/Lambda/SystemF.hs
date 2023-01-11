module Language.Lambda.SystemF (
  evalText,
  runEvalText,
  execEvalText,
  unsafeExecEvalText,
  defaultUniques,
  defaultTyUniques,
  mkState,

  module Language.Lambda.SystemF.Expression,
  module Language.Lambda.SystemF.Parser,
  module Language.Lambda.SystemF.State
  ) where

import Language.Lambda.Shared.Errors
import Language.Lambda.Shared.UniqueSupply (defaultUniques, defaultTyUniques)
import Language.Lambda.SystemF.Eval (evalExpr)
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Parser
import Language.Lambda.SystemF.State
import Language.Lambda.SystemF.TypeCheck

import Control.Monad.Except
import RIO
import qualified RIO.Text as Text
import qualified RIO.Map as Map

evalText
  :: Text
  -> Typecheck Text (TypedExpr Text)
evalText = either throwParseError processExpr . parseExpr
    where throwParseError = throwError . ParseError . Text.pack . show

runEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (TypedExpr Text, TypecheckState Text)
runEvalText input globals' = runTypecheck (evalText input) (mkState globals')

execEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (TypedExpr Text)
execEvalText input globals' = execTypecheck (evalText input) (mkState globals')

unsafeExecEvalText
  :: Text
  -> Globals Text
  -> TypedExpr Text
unsafeExecEvalText input globals' = unsafeExecTypecheck (evalText input) (mkState globals')

mkState :: Globals Text -> TypecheckState Text
mkState globals' = TypecheckState globals' defaultUniques defaultTyUniques

processExpr :: SystemFExpr Text -> Typecheck Text (TypedExpr Text)
processExpr (Let n expr) = tcAndEval expr >>= addBinding n
processExpr expr = tcAndEval expr

tcAndEval :: SystemFExpr Text -> Typecheck Text (TypedExpr Text)
tcAndEval expr = do
  ty <- typecheck expr
  reduced <- evalExpr expr

  pure $ TypedExpr reduced ty

addBinding :: Text -> TypedExpr Text -> Typecheck Text (TypedExpr Text)
addBinding name expr = modifyGlobals (Map.insert name expr) >> pure expr
