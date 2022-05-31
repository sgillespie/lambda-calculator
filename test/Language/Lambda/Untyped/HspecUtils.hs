{-# LANGUAGE NoImplicitPrelude #-}
module Language.Lambda.Untyped.HspecUtils where

import RIO
import Test.Hspec

import Language.Lambda.Shared.Errors
import Language.Lambda.Untyped

shouldEvalTo :: Text -> Text -> Expectation
shouldEvalTo s1 = shouldBe (eval s1) . eval

eval :: Text -> Either LambdaException (LambdaExpr Text)
eval input = execEval (evalText input) (mkEvalState defaultUniques)
