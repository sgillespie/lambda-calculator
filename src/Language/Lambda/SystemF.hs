module Language.Lambda.SystemF (
  Globals(),
  evalText,

  module Language.Lambda.SystemF.Expression,
  module Language.Lambda.SystemF.Parser
  ) where

import RIO
import Text.Parsec

import qualified Data.Map as Map

import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Parser

type Globals = Map.Map String (SystemFExpr String String)

evalText
  :: Globals
  -> Text
  -> Either ParseError (SystemFExpr Text Text, Globals)
evalText globals = fmap (, globals) . parseExpr
