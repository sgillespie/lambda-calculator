{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda (
  EvalState(..),
  LambdaExpr(..),
  PrettyPrint(..),
  evalString,
  mkEvalState,
  parseExpr,
  uniques,
  ) where

import Control.Monad
import Prelude
import Text.Parsec

import Language.Lambda.Eval
import Language.Lambda.Expression
import Language.Lambda.Parser
import Language.Lambda.State (Eval(), mkEvalState)
import Language.Lambda.Util.PrettyPrint

evalString :: String -> Eval String (Either ParseError (LambdaExpr String))
evalString str = do
  let expr = parseExpr str

  case expr of
    Left err -> return $ Left err
    Right expr' -> do
      res <- evalExpr expr'
      return $ Right res

uniques :: [String]
uniques = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
  where suffix = "" : map show [(0::Int)..]
