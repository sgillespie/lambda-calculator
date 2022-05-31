{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Language.Lambda.UntypedSpec where

import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import Test.Hspec

import Language.Lambda.Untyped
import Language.Lambda.Untyped.HspecUtils

spec :: Spec
spec = do
  describe "evalText" $ do
    it "evaluates simple text" $ do
      eval "x" `shouldBe` Right (Var "x")
      eval "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))
      eval "f y" `shouldBe` Right (App (Var "f") (Var "y"))

    it "reduces simple applications" $
      eval "(\\x .x) y" `shouldBe` Right (Var "y")

    it "reduces applications with nested redexes" $
      eval "(\\f x. f x) (\\y. y)" `shouldBe` Right (Abs "x" (Var "x"))

  describe "runEvalText" $ do
    let runEvalText' input = fst <$> runEvalText input Map.empty
    
    it "evaluates simple strings" $ do
      runEvalText' "x" `shouldBe` Right (Var "x")
      runEvalText' "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))
      runEvalText' "f y" `shouldBe` Right (App (Var "f") (Var "y"))

    it "reduces simple applications" $
      runEvalText' "(\\x .x) y" `shouldBe` Right (Var "y")

    it "reduces applications with nested redexes" $
      runEvalText' "(\\f x. f x) (\\y. y)" `shouldBe` Right (Abs "x" (Var "x"))

  describe "execEvalText" $ do
    let execEvalText' input = execEvalText input Map.empty
    
    it "evaluates simple texts" $ do
      execEvalText' "x" `shouldBe` Right (Var "x")
      execEvalText' "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))
      execEvalText' "f y" `shouldBe` Right (App (Var "f") (Var "y"))

    it "reduces simple applications" $
      execEvalText' "(\\x .x) y" `shouldBe` Right (Var "y")

    it "reduces applications with nested redexes" $
      execEvalText' "(\\f x. f x) (\\y. y)" `shouldBe` Right (Abs "x" (Var "x"))

  describe "unsafeExecEvalText" $ do
    let unsafeExecEvalText' input = unsafeExecEvalText input Map.empty
    
    it "evaluates simple texts" $ do
      unsafeExecEvalText' "x" `shouldBe` Var "x"
      unsafeExecEvalText' "\\x. x" `shouldBe` Abs "x" (Var "x")
      unsafeExecEvalText' "f y" `shouldBe` App (Var "f") (Var "y")

    it "reduces simple applications" $
      unsafeExecEvalText' "(\\x .x) y" `shouldBe` Var "y"

    it "reduces applications with nested redexes" $
      unsafeExecEvalText' "(\\f x. f x) (\\y. y)" `shouldBe` Abs "x" (Var "x")

  describe "defaultUniques" $ do
    let alphabet = reverse ['a'..'z']
        len = length alphabet
    
    it "starts with plain alphabet" $
      take len defaultUniques `shouldBe` map (`Text.cons` Text.empty) alphabet

    it "adds index afterwards" $
      take len (drop len defaultUniques)
        `shouldBe` map (`Text.cons` Text.singleton '0') alphabet
