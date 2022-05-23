{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda (
  EvalState(..),
  evalString,
  mkEvalState,  
  parseExpr,
  uniques,

  module Language.Lambda.Expression
  ) where

import Control.Monad
import Prelude
import Text.Parsec

import Language.Lambda.Eval
import Language.Lambda.Expression
import Language.Lambda.Parser
import Language.Lambda.State (Eval(), mkEvalState)

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
