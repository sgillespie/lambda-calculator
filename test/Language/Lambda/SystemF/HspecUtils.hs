module Language.Lambda.SystemF.HspecUtils where

import RIO
import RIO.Text (toUpper)
import Test.Hspec

import Language.Lambda.Shared.Errors
import Language.Lambda.SystemF

shouldEvalTo :: Text -> Text -> Expectation
shouldEvalTo s = shouldBe (eval s) . eval

eval :: Text -> Either LambdaException (Result Text)
eval input = execTypecheck (evalText input) (mkTypecheckState uniqs)
  where uniqs = map toUpper defaultUniques

shouldBeRight
  :: (Show l, Show r, Eq l, Eq r)
  => Either l r
  -> r
  -> Expectation
shouldBeRight res = (res `shouldBe`) . Right

shouldBeLeft
  :: (Show l, Show r, Eq l, Eq r)
  => Either l r
  -> l
  -> Expectation
shouldBeLeft res = (res `shouldBe`) . Left
