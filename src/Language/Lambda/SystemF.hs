module Language.Lambda.SystemF (
  Globals(),
  evalText,
  runEvalText,
  execEvalText,
  unsafeExecEvalText,
  defaultUniques,
  defaultTyUniques,


  module Language.Lambda.SystemF.Expression,
  module Language.Lambda.SystemF.Parser,
  module Language.Lambda.SystemF.State
  ) where

import Language.Lambda.Shared.Errors
import Language.Lambda.Shared.UniqueSupply (defaultUniques)
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Parser
import Language.Lambda.SystemF.State
import Language.Lambda.SystemF.TypeCheck

import Control.Monad.Except
import RIO
import qualified RIO.Text as Text
import qualified Data.Map as Map

type Globals name = Map.Map name (TypedExpr name)

evalText
  :: Text
  -> Typecheck Text (TypedExpr Text)
evalText = either throwParseError typecheckExpr . parseExpr
    where throwParseError = throwError . ParseError . Text.pack . show
          typecheckExpr expr = TypedExpr expr <$> typecheck expr

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

defaultTyUniques :: [Text]
defaultTyUniques = map Text.toUpper defaultUniques

mkState :: Globals Text -> TypecheckState Text
mkState globals' = TypecheckState context' defaultUniques defaultTyUniques
  where context' = Map.map (^. _ty) globals'
