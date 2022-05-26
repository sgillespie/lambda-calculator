{-# LANGUAGE NoImplicitPrelude #-}
module Language.Lambda.Untyped.HspecUtils where

import RIO
import RIO.State (evalState)
import Test.Hspec

import Language.Lambda.Untyped
import Language.Lambda.Untyped.Parser (ParseError())

shouldEvalTo :: String -> String -> Expectation
shouldEvalTo s1 = shouldBe (eval s1) . eval

eval :: String -> Either ParseError (LambdaExpr String)
eval input = evalState (evalString input) (mkEvalState uniques)
