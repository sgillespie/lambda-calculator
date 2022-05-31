{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Language.Lambda.Untyped.ExpressionSpec where

import Language.Lambda.Untyped.Expression

import RIO
import Test.Hspec

spec :: Spec
spec = describe "prettyPrint" $ do
    let prettyPrint' :: LambdaExpr Text -> Text
        prettyPrint' = prettyPrint
  
    it "prints simple variables" $
      prettyPrint' (Var "x") `shouldBe` "x"

    it "prints simple abstractions" $
      prettyPrint' (Abs "x" (Var "x")) `shouldBe` "λx. x"

    it "prints simple applications" $
      prettyPrint' (App (Var "a") (Var "b"))
        `shouldBe` "a b"

    it "prints simple let expressions" $
      prettyPrint' (Let "x" (Var "y")) `shouldBe` "let x = y"

    it "prints nested abstractions" $
      prettyPrint' (Abs "f" (Abs "x" (Abs "y" (Var "x"))))
        `shouldBe` "λf x y. x"

    it "prints nested applications" $
      prettyPrint' (App (App (Var "f") (Var "x")) (Var "y"))
        `shouldBe` "f x y"

    it "prints parenthesized applications" $ do
      prettyPrint' (App (Var "f") (App (Var "x") (Var "y")))
        `shouldBe` "f (x y)"

      prettyPrint' (App (Abs "x" (Var "x")) (Var "y"))
        `shouldBe` "(λx. x) y"

      prettyPrint' (App (Var "x") (Abs "f" (Var "f")))
        `shouldBe` "x (λf. f)"
      
      prettyPrint' (App (Abs "f" (Var "f")) (Abs "g" (Var "g")))
        `shouldBe` "(λf. f) (λg. g)"

    it "prints complex let expressions" $
      prettyPrint' (Let "x" (Abs "a" (Abs "b" (App (Var "a") (Var "b")))))
        `shouldBe` "let x = λa b. a b"
