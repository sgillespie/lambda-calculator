module Language.Lambda.HspecUtils where

import RIO.State (evalState)
import Test.Hspec

import Language.Lambda
import Language.Lambda.Parser (ParseError())

shouldEvalTo :: String -> String -> Expectation
shouldEvalTo s1 = shouldBe (eval s1) . eval

eval :: String -> Either ParseError (LambdaExpr String)
eval input = evalState (evalString input) (mkEvalState uniques)
