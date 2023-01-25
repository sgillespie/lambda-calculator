module Language.Lambda.SystemF (
  evalText,
  typecheckText,
  runEvalText,
  runTypecheckText,
  execEvalText,
  execTypecheckText,
  unsafeExecEvalText,
  unsafeExecTypecheckText,
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

typecheckText
  :: Text
  -> Typecheck Text (Ty Text)
typecheckText = either throwParseError typecheck . parseExpr
    where throwParseError = throwError . ParseError . Text.pack . show

runEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (TypedExpr Text, TypecheckState Text)
runEvalText input globals' = runTypecheck (evalText input) (mkState globals')

runTypecheckText
  :: Text
  -> Globals Text
  -> Either LambdaException (Ty Text, TypecheckState Text)
runTypecheckText input globals'
  = runTypecheck (typecheckText input) (mkState globals')

execEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (TypedExpr Text)
execEvalText input globals'
  = execTypecheck (evalText input) (mkState globals')

execTypecheckText
  :: Text
  -> Globals Text
  -> Either LambdaException (Ty Text)
execTypecheckText input globals'
  = execTypecheck (typecheckText input) (mkState globals')

unsafeExecEvalText
  :: Text
  -> Globals Text
  -> TypedExpr Text
unsafeExecEvalText input globals'
  = unsafeExecTypecheck (evalText input) (mkState globals')

unsafeExecTypecheckText
  :: Text
  -> Globals Text
  -> Ty Text
unsafeExecTypecheckText input globals'
  = unsafeExecTypecheck (typecheckText input) (mkState globals')

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
