module Language.Lambda.SystemF (
  Globals(),
  Result(..),
  _expr,
  _ty,
  evalText,
  defaultUniques,

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

type Globals name = Map.Map name (SystemFExpr name)

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
evalText text = do
  case parseExpr text of
    Left err -> throwError $ ParseError $ Text.pack $ show err
    Right res -> Result res <$> typecheck res
