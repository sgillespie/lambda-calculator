module Language.Lambda.SystemF (
  Globals(),
  Result(..),
  _expr,
  _ty,
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
import Prettyprinter
import RIO
import qualified RIO.Text as Text
import qualified Data.Map as Map

type Globals name = Map.Map name (Result name)

data Result name = Result
  { expr :: SystemFExpr name,
    ty :: Ty name
  } deriving (Eq, Show)

_expr :: Lens' (Result name) (SystemFExpr name)
_expr = lens expr (\res expr -> res { expr = expr })

_ty :: Lens' (Result name) (Ty name)
_ty = lens ty (\res ty -> res { ty = ty })

instance Pretty name => Pretty (Result name) where
  pretty Result{..} = pretty expr <+> colon <+> pretty ty

evalText
  :: Text
  -> Typecheck Text (Result Text)
evalText = either throwParseError typecheckExpr . parseExpr
    where throwParseError = throwError . ParseError . Text.pack . show
          typecheckExpr expr = Result expr <$> typecheck expr

runEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (Result Text, TypecheckState Text)
runEvalText input globals' = runTypecheck (evalText input) (mkState globals')

execEvalText
  :: Text
  -> Globals Text
  -> Either LambdaException (Result Text)
execEvalText input globals' = execTypecheck (evalText input) (mkState globals')

unsafeExecEvalText
  :: Text
  -> Globals Text
  -> Result Text
unsafeExecEvalText input globals' = unsafeExecTypecheck (evalText input) (mkState globals')

defaultTyUniques :: [Text]
defaultTyUniques = map Text.toUpper defaultUniques

mkState :: Globals Text -> TypecheckState Text
mkState globals' = TypecheckState context' defaultUniques defaultTyUniques
  where context' = Map.map (^. _ty) globals'
