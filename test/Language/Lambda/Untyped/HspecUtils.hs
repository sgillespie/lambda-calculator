{-# LANGUAGE NoImplicitPrelude #-}
module Language.Lambda.Untyped.HspecUtils where

import RIO
import Test.Hspec

import Language.Lambda.Shared.Errors
import Language.Lambda.Untyped
import Language.Lambda.Untyped.State hiding (uniques)

shouldEvalTo :: String -> String -> Expectation
shouldEvalTo s1 = shouldBe (eval s1) . eval

eval :: String -> Either LambdaException (LambdaExpr String)
eval input = execEval (evalString input) (mkEvalState defaultUniques)
