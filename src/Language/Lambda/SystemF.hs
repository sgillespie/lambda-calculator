module Language.Lambda.SystemF (
  Globals(),
  evalText,

  module Language.Lambda.SystemF.Expression,
  module Language.Lambda.SystemF.Parser,
  module Language.Lambda.SystemF.State
  ) where

import Language.Lambda.Shared.Errors
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Parser
import Language.Lambda.SystemF.State

import Control.Monad.Except
import Prettyprinter
import RIO
import qualified RIO.Text as Text
import qualified Data.Map as Map

type Globals = Map.Map Text (SystemFExpr Text Text)

data Result name = Result
  { expr :: SystemFExpr name name,
    ty :: Ty name
  } deriving (Eq, Show)

instance Pretty name => Pretty (Result name) where
  pretty Result{..} = pretty expr <+> colon <+> pretty ty

-- Procedure
--  1. Parse expression
--  2. Typecheck expression
--  3. Evaluate expression
--  4. Return reduced expression along with its type
evalText
  :: Text
  -> Typecheck Text (Result Text)
evalText text = case parseExpr text of
  Left err -> throwError $ ParseError $ Text.pack $ show err
  Right res -> return $ Result res (TyVar "A")
