{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda (
  EvalState(..),
  LambdaExpr(..),
  PrettyPrint(..),
  evalExpr,
  evalString,
  evalStringM,
  mkEvalState,
  parseExpr,
  uniques,
  ) where

import Control.Monad
import Prelude
import Text.Parsec

import qualified Data.Map as Map

import Language.Lambda.Eval
import Language.Lambda.Expression
import Language.Lambda.Parser
import Language.Lambda.Util.PrettyPrint

type Globals = Map.Map String (LambdaExpr String)

evalString :: Globals
           -> String
           -> Either ParseError (LambdaExpr String, Globals)
evalString globals str = evalExpr globals uniques <$> parseExpr str

evalStringM :: String -> MonadLambda String (Either ParseError (LambdaExpr String))
evalStringM str = do
  let expr = parseExpr str

  case expr of
    Left err -> return $ Left err
    Right expr' -> do
      res <- evalExprM expr'
      return $ Right res

uniques :: [String]
uniques = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
  where suffix = "" : map show [(0::Int)..]
