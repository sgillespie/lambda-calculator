module Language.Lambda.SystemF.ExpressionSpec where

import RIO
import Test.Hspec

import Language.Lambda.SystemF.Expression

spec :: Spec
spec = describe "prettyPrint" $ do
  let prettyPrint' :: SystemFExpr Text -> Text
      prettyPrint' = prettyPrint

      prettyPrintTy :: Ty Text -> Text
      prettyPrintTy = prettyPrint
  
  it "prints simple variables" $
    prettyPrint' (Var "x") `shouldBe` "x"

  it "prints annotated variables" $
    prettyPrint' (VarAnn "x" (TyVar "T")) `shouldBe` "x:T"

  it "prints simple applications" $
    prettyPrint' (App (Var "a") (Var "b")) `shouldBe` "a b"

  it "prints simple abstractions" $ 
    prettyPrint' (Abs "x" (TyVar "T") (Var "x")) `shouldBe` "λ x:T. x"

  it "prints simple type abstractions" $
    prettyPrint' (TyAbs "X" (Var "x")) `shouldBe` "Λ X. x"

  it "prints simple type applications" $ 
    prettyPrint' (TyApp (Var "t") (TyVar "T")) `shouldBe` "t [T]"

  it "prints simple let expressions" $
    prettyPrint' (Let "x" (Var "y")) `shouldBe` "let x = y"

  it "prints annotated variables with composite types" $
    prettyPrint' (VarAnn "x" (TyArrow (TyVar "T") (TyVar "V"))) `shouldBe` "x:(T->V)"

  it "prints nested abstractions" $
    prettyPrint' (Abs "f" (TyVar "F") (Abs "x" (TyVar "X") (Var "x")))
      `shouldBe` "λ f:F x:X. x"

  it "prints abstractions with composite types" $ do
    prettyPrint' (Abs "f" (TyArrow (TyVar "X") (TyVar "Y")) (Var "f"))
      `shouldBe ` "λ f:(X->Y). f"

    prettyPrint' (Abs "f" (TyArrow (TyVar "X") (TyArrow (TyVar "Y") (TyVar "Z"))) (Var "f"))
      `shouldBe ` "λ f:(X->Y->Z). f"

  it "prints nested type abstractions" $
    prettyPrint' (TyAbs "A" (TyAbs "B" (Var "x")))
      `shouldBe` "Λ A B. x"

  it "prints nested applications" $
    prettyPrint' (App (App (Var "f") (Var "x")) (Var "y"))
      `shouldBe` "f x y"

  it "prints parenthesized applications" $ do
    prettyPrint' (App (Var "w") (App (Var "x") (Var "y")))
      `shouldBe` "w (x y)"

    prettyPrint' (App (Abs "t" (TyVar "T") (Var "t")) (Var "x"))
      `shouldBe` "(λ t:T. t) x"

    prettyPrint' (App (Abs "f" (TyVar "F") (Var "f")) (Abs "g" (TyVar "G") (Var "g")))
      `shouldBe` "(λ f:F. f) (λ g:G. g)"

  it "prints simple types" $
    prettyPrintTy (TyVar "X") `shouldBe` "X"

  it "print simple arrow types" $
    prettyPrintTy (TyArrow (TyVar "A") (TyVar "B")) `shouldBe` "A -> B"

  it "prints simple forall types" $
    prettyPrintTy (TyForAll "X" (TyVar "X")) `shouldBe` "forall X. X"

  it "prints chained arrow types" $
    prettyPrintTy (TyArrow (TyVar "X") (TyArrow (TyVar "Y") (TyVar "Z")))
      `shouldBe` "X -> Y -> Z"

  it "prints nested arrow types" $
    prettyPrintTy (TyArrow (TyArrow (TyVar "T") (TyVar "U")) (TyVar "V"))
      `shouldBe` "(T -> U) -> V"

  it "prints complex forall types" $
    prettyPrintTy (TyForAll "A" (TyArrow (TyVar "A") (TyVar "A")))
      `shouldBe` "forall A. A -> A"

  it "prints nested forall types" $
    prettyPrintTy (TyForAll "W" 
                  (TyForAll "X" 
                    (TyArrow (TyVar "W") (TyArrow (TyVar "X") (TyVar "Y")))))
      `shouldBe` "forall W. forall X. W -> X -> Y"

