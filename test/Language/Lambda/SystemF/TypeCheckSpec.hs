module Language.Lambda.SystemF.TypeCheckSpec (spec) where

import Language.Lambda.Shared.Errors
import Language.Lambda.Shared.UniqueSupply (defaultUniques)
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.State
import Language.Lambda.SystemF.TypeCheck
import Language.Lambda.SystemF.HspecUtils

import Data.Either
import Data.Map
import Lens.Micro
import RIO
import Test.Hspec

tc
  :: [Text]
  -> [(Text, Ty Text)]
  -> SystemFExpr Text
  -> Either LambdaException (Ty Text, TypecheckState Text)
tc uniqs ctx expr = runTypecheck (typecheck expr) initialState
  where initialState = TypecheckState (fromList ctx) defaultUniques uniqs

tc'
  :: [Text]
  -> [(Text, Ty Text)]
  -> SystemFExpr Text
  -> Either LambdaException (Ty Text)
tc' uniqs ctx expr = over _Right fst $ tc uniqs ctx expr

spec :: Spec
spec = describe "typecheck" $ do
  it "typechecks simple variables in context" $
    tc' [] [("x", TyVar "X")] (Var "x") `shouldBe` Right (TyVar "X")

  it "typechecks simple variables not in context" $ 
    tc' ["A"] [] (Var "x") `shouldBe` Right (TyVar "A")

  it "typechecks annotated variables in context" $
    tc' [] [("x", TyVar "X")] (VarAnn "x" (TyVar "X"))
      `shouldBe` Right (TyVar "X")

  it "typechecks annotated variables in context" $
    tc' [] [] (VarAnn "x" (TyVar "X"))
      `shouldBe` Right (TyVar "X")

  it "typechecks simple abstractions" $
    tc' [] [] (Abs "x" (TyVar "A") (Var "x")) 
      `shouldBe` Right (TyArrow (TyVar "A") (TyVar "A"))

  it "abstractions don't modify the context" $ do
    let res = tc [] [] (Abs "x" (TyVar "A") (Var "x"))

    over _Right (^. (_2 . _context)) res
      `shouldBeRight` empty

  it "typechecks simple applications" $ do
    let ctx = [
          ("f", TyArrow (TyVar "T") (TyVar "U")),
          ("a", TyVar "T")
          ]

    tc' [] ctx (App (Var "f") (Var "a")) `shouldBe` Right (TyVar "U")

  it "typechecks application of annotated variables" $ do
    let f = VarAnn "f" (TyArrow (TyVar "T") (TyVar "T"))
        x = VarAnn "x" (TyVar "T")
    
    tc' [] [] (App f x) `shouldBe` Right (TyVar "T")

  it "typechecks application of annotated variable to abstraction" $ do
    let f = Abs "t" (TyVar "T")  (Var "t")
        x = VarAnn "x" (TyVar "T")
    
    tc' [] [] (App f x) `shouldBe` Right (TyVar "T")

  it "annotated variable with wrong type fails" $ 
    tc' [] [("x", TyVar "T")] (VarAnn "x" (TyVar "X"))
      `shouldSatisfy` isLeft

  it "apply variable to variable fails" $ do
    let ctx = [
          ("a", TyVar "A"),
          ("b", TyVar "B")
          ]

    tc' ["C"] ctx (App (Var "a") (Var "b")) 
      `shouldSatisfy` isLeft

  it "apply arrow to variable of wrong type fails" $ do
    let ctx = [
          ("f", TyArrow (TyVar "F") (TyVar "G")),
          ("b", TyVar "B")
          ]

    tc' [] ctx (App (Var "f") (Var "b")) `shouldSatisfy` isLeft

  it "typechecks simple type abstractions" $
    tc' ["A"] [] (TyAbs "X" (Var "x")) `shouldBe` Right (TyForAll "X" (TyVar "A"))

  it "typechecks type abstractions with simple abstraction" $
    tc' [] [] (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) 
      `shouldBe` Right (TyForAll "X" (TyArrow (TyVar "X") (TyVar "X")))

  it "typechecks type abstractions with application" $
    tc' [] [("y", TyVar "Y")] 
      (App (TyApp (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) (TyVar "Y")) 
           (Var "y"))
      `shouldBe` Right (TyVar "Y")

  it "type abstractions don't modify the context" $ do
    let res = tc ["A"] [] (TyAbs "X" (Var "x"))

    over _Right (^. (_2 . _context)) res
      `shouldBeRight` empty

  it "typechecks simple type applications" $
    tc' [] [("x", TyVar "A")] (TyApp (TyAbs "X" (Var "x")) (TyVar "X"))
      `shouldBe` Right (TyVar "A")

  it "typechecks type applications with simple abstraction" $
    tc' [] [] (TyApp (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) (TyVar "Y"))
      `shouldBe` Right (TyArrow (TyVar "Y") (TyVar "Y"))
