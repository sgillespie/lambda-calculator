module Language.Lambda.SystemF (
  Globals(..),
  PrettyPrint(..),
  SystemFExpr(..),
  evalString,
  parseExpr
  ) where

import Prelude
import Text.Parsec

import qualified Data.Map as Map

import Language.Lambda.Util.PrettyPrint
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Parser

type Globals = Map.Map String (SystemFExpr String String)

evalString :: Globals
           -> String
           -> Either ParseError (SystemFExpr String String, Globals)
evalString globals = fmap (, globals) . parseExpr
