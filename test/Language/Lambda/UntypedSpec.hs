module Language.Lambda.UntypedSpec where

import qualified RIO.Map as Map
import Test.Hspec

import Language.Lambda.Untyped
import Language.Lambda.Untyped.HspecUtils

spec :: Spec
spec = do
  describe "evalString" $ do
    it "evaluates simple strings" $ do
      eval "x" `shouldBe` Right (Var "x")
      eval "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))
      eval "f y" `shouldBe` Right (App (Var "f") (Var "y"))

    it "reduces simple applications" $
      eval "(\\x .x) y" `shouldBe` Right (Var "y")

    it "reduces applications with nested redexes" $
      eval "(\\f x. f x) (\\y. y)" `shouldBe` Right (Abs "x" (Var "x"))

  describe "runEvalString" $ do
    let runEvalString' input = fst <$> runEvalString input Map.empty
    
    it "evaluates simple strings" $ do
      runEvalString' "x" `shouldBe` Right (Var "x")
      runEvalString' "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))
      runEvalString' "f y" `shouldBe` Right (App (Var "f") (Var "y"))

    it "reduces simple applications" $
      runEvalString' "(\\x .x) y" `shouldBe` Right (Var "y")

    it "reduces applications with nested redexes" $
      runEvalString' "(\\f x. f x) (\\y. y)" `shouldBe` Right (Abs "x" (Var "x"))

  describe "execEvalString" $ do
    let execEvalString' input = execEvalString input Map.empty
    
    it "evaluates simple strings" $ do
      execEvalString' "x" `shouldBe` Right (Var "x")
      execEvalString' "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))
      execEvalString' "f y" `shouldBe` Right (App (Var "f") (Var "y"))

    it "reduces simple applications" $
      execEvalString' "(\\x .x) y" `shouldBe` Right (Var "y")

    it "reduces applications with nested redexes" $
      execEvalString' "(\\f x. f x) (\\y. y)" `shouldBe` Right (Abs "x" (Var "x"))

  describe "unsafeExecEvalString" $ do
    let unsafeExecEvalString' input = unsafeExecEvalString input Map.empty
    
    it "evaluates simple strings" $ do
      unsafeExecEvalString' "x" `shouldBe` Var "x"
      unsafeExecEvalString' "\\x. x" `shouldBe` Abs "x" (Var "x")
      unsafeExecEvalString' "f y" `shouldBe` App (Var "f") (Var "y")

    it "reduces simple applications" $
      unsafeExecEvalString' "(\\x .x) y" `shouldBe` Var "y"

    it "reduces applications with nested redexes" $
      unsafeExecEvalString' "(\\f x. f x) (\\y. y)" `shouldBe` Abs "x" (Var "x")

  describe "defaultUniques" $ do
    let alphabet = reverse ['a'..'z']
        len = length alphabet
    
    it "starts with plain alphabet" $
      take len defaultUniques `shouldBe` map (:[]) alphabet

    it "adds index afterwards" $
      take len (drop len defaultUniques) `shouldBe` map (:['0']) alphabet
