module Language.Lambda.SystemF (
  Globals(),
  evalText,

  module Language.Lambda.SystemF.Expression,
  module Language.Lambda.SystemF.Parser,
  module Language.Lambda.SystemF.State
  ) where

import Control.Monad.Except
import RIO
import qualified RIO.Text as Text
import qualified Data.Map as Map

import Language.Lambda.Shared.Errors
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Parser
import Language.Lambda.SystemF.State

type Globals = Map.Map String (SystemFExpr String String)

evalText :: Text -> Typecheck Text (SystemFExpr Text Text)
evalText text = case parseExpr text of
  Left err -> throwError $ ParseError $ Text.pack $ show err
  Right res -> return res

