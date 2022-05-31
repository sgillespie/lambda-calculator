{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda.Untyped (
  evalString,
  defaultUniques,

  module Language.Lambda.Untyped.Expression,
  module Language.Lambda.Untyped.Eval,
  module Language.Lambda.Untyped.Parser,
  module Language.Lambda.Untyped.State
  ) where

import Control.Monad.Except
import Data.Either
import RIO
import RIO.Text (pack)

import Language.Lambda.Shared.Errors
import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Expression
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.State

evalString :: String -> Eval String (LambdaExpr String)
evalString = either throwParseError evalExpr . parseExpr
  where throwParseError = throwError . ParseError . pack . show

defaultUniques :: [String]
defaultUniques = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
  where suffix = "" : map show [(0::Int)..]
