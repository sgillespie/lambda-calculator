module Language.Lambda.SystemF.HspecUtils where

import Language.Lambda.Shared.Errors
import Language.Lambda.SystemF

import RIO
import Test.Hspec
import qualified RIO.Map as Map

shouldEvalTo :: Text -> Text -> Expectation
shouldEvalTo s = shouldBe (eval s) . eval

shouldTypecheckTo :: Text -> Text -> Expectation
shouldTypecheckTo = shouldTypecheckToWithGlobals []

shouldTypecheckToWithGlobals :: [(Text, TypedExpr Text)] -> Text -> Text -> Expectation
shouldTypecheckToWithGlobals globals expr ty = typecheck' globals expr `shouldHaveType` ty

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

shouldHaveType
  :: Either LambdaException (Ty Text)
  -> Text
  -> Expectation
shouldHaveType res tyRepr = case parseType tyRepr of
    Left err -> expectationFailure $
      "Could not parse type " <> show tyRepr <> ": " <> show err
    Right ty -> res `shouldBe` Right ty

shouldFailWith
  :: Either LambdaException (Ty Text)
  -> Selector LambdaException
  -> Expectation
shouldFailWith res selector = case res of
  Left err -> err `shouldSatisfy` selector
  Right res' -> expectationFailure $
        "did not get expected failure: " <> show res'

eval :: Text -> Either LambdaException (TypedExpr Text)
eval input = execTypecheck (evalText input) initialState
  where initialState = mkTypecheckState defaultUniques defaultTyUniques

typecheck' :: [(Text, TypedExpr Text)] -> Text -> Either LambdaException (Ty Text)
typecheck' globals input = execTypecheck (typecheckText input) initialState
  where initialState = TypecheckState (Map.fromList globals) defaultUniques defaultTyUniques

runTypecheck'
  :: [(Text, TypedExpr Text)]
  -> Text
  -> Either LambdaException (Ty Text, TypecheckState Text)
runTypecheck' globals input = runTypecheck (typecheckText input) initialState
  where initialState = TypecheckState (Map.fromList globals) defaultUniques defaultTyUniques
