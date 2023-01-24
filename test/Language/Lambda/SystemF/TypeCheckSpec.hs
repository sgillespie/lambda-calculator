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
  -> [(Text, TypedExpr Text)]
  -> SystemFExpr Text
  -> Either LambdaException (Ty Text, TypecheckState Text)
tc uniqs globals' expr = runTypecheck (typecheck expr) initialState
  where initialState = TypecheckState (fromList globals') defaultUniques uniqs

tc'
  :: [Text]
  -> [(Text, TypedExpr Text)]
  -> SystemFExpr Text
  -> Either LambdaException (Ty Text)
tc' uniqs globals' expr = over _Right fst $ tc uniqs globals' expr

spec :: Spec
spec = describe "typecheck" $ do
  let someGlobal = ("x", TypedExpr (Var "y") (TyVar "X"))
  
  it "typechecks simple variables in context" $
    tc' [] [someGlobal] (Var "x") `shouldBe` Right (TyVar "X")

  it "typechecks simple variables not in context" $ 
    tc' ["A"] [] (Var "x") `shouldBe` Right (TyVar "A")

  it "typechecks annotated variables in context" $
    tc' [] [someGlobal] (VarAnn "x" (TyVar "X"))
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
    let globals' = [
          ("f", TypedExpr (Var "f") $ TyArrow (TyVar "T") (TyVar "U")),
          ("a", TypedExpr (Var "a") $ TyVar "T")
          ]

    tc' [] globals' (App (Var "f") (Var "a")) `shouldBe` Right (TyVar "U")

  it "typechecks simple let expressions" $ do
    tc' ["A"] [] (Let "x" (Var "y")) `shouldBeRight` TyVar "A"

  it "typechecks application of annotated variables" $ do
    let f = VarAnn "f" (TyArrow (TyVar "T") (TyVar "T"))
        x = VarAnn "x" (TyVar "T")
    
    tc' [] [] (App f x) `shouldBe` Right (TyVar "T")

  it "typechecks application of annotated variable to abstraction" $ do
    let f = Abs "t" (TyVar "T")  (Var "t")
        x = VarAnn "x" (TyVar "T")
    
    tc' [] [] (App f x) `shouldBe` Right (TyVar "T")

  it "annotated variable with wrong type fails" $ 
    tc' [] [someGlobal] (VarAnn "x" (TyVar "Y"))
      `shouldSatisfy` isLeft

  it "apply variable to variable fails" $ do
    let globals'
          = [ ("a", TypedExpr (Var "a") (TyVar "A")),
              ("b", TypedExpr (Var "b") (TyVar "B"))
            ]

    tc' ["C"] globals' (App (Var "a") (Var "b")) 
      `shouldSatisfy` isLeft

  it "nested let fails" $ do
    tc' ["A"] [] (Abs "x" (TyVar "T") (Let "y" (Var "z")))
      `shouldSatisfy` isLeft

  it "apply arrow to variable of wrong type fails" $ do
    let globals' = [
          ("f", TypedExpr (Var "f") (TyArrow (TyVar "F") (TyVar "G"))),
          ("b", TypedExpr (Var "b") (TyVar "B"))
          ]

    tc' [] globals' (App (Var "f") (Var "b")) `shouldSatisfy` isLeft

  it "type application to concrete type fails" $ do
    let globals' = [("x", TypedExpr (Var "y") (TyVar "Y"))]
    
    tc' [] [] (TyApp (VarAnn "x" (TyVar "T")) (TyVar "U")) `shouldSatisfy` isLeft
    tc' [] globals' (TyApp (Var "x") (TyVar "U")) `shouldSatisfy` isLeft

  it "typechecks simple type abstractions" $
    tc' ["A"] [] (TyAbs "X" (Var "x")) `shouldBe` Right (TyForAll "X" (TyVar "A"))

  it "typechecks type abstractions with simple abstraction" $
    tc' [] [] (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) 
      `shouldBe` Right (TyForAll "X" (TyArrow (TyVar "X") (TyVar "X")))

  it "typechecks type abstractions with application" $
    tc' [] [("y", TypedExpr (Var "y") (TyVar "Y"))] 
      (App (TyApp (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) (TyVar "Y")) 
           (Var "y"))
      `shouldBe` Right (TyVar "Y")

  it "type abstractions don't modify the context" $ do
    let res = tc ["A"] [] (TyAbs "X" (Var "x"))

    over _Right (^. (_2 . _context)) res
      `shouldBeRight` empty

  it "typechecks simple type applications" $ do
    let globals' = [("x", TypedExpr (Var "x") $ TyVar "A")]
        tyApp inner = TyApp (TyAbs "X" inner) (TyVar "T")
    
    tc' [] globals' (tyApp $ Var "x") `shouldBeRight` TyVar "A"
    tc' [] globals' (tyApp $ VarAnn "z" (TyVar "X")) `shouldBeRight` TyVar "T"

  it "typechecks type applications with simple abstraction" $ do
    let tyApp inner = TyApp
          (TyAbs "X" (Abs "x" (TyVar "X") inner))
          (TyVar "Y")
    
    tc' [] [] (tyApp $ Var "x") `shouldBeRight` TyArrow (TyVar "Y") (TyVar "Y")

  it "typechecks polymorphic arguments" $ do
    let expr = Abs "x" (TyForAll "T" (TyVar "T")) $ Var "x"
        expr2 = Abs "x" (TyForAll "T" (TyArrow (TyVar "T") (TyVar "U"))) $ Var "x"
        expr3 = Abs "x" (TyArrow (TyVar "U") (TyForAll "T" (TyVar "T"))) $ Var "x"
        expr4 = Abs "x" (TyForAll "T" (TyVar "T")) $ VarAnn "x" (TyVar "T")

    tc' [] [] expr `shouldBeRight` TyForAll "T" (TyArrow (TyVar "T") (TyVar "T"))
    tc' [] [] expr2 `shouldBeRight`
      TyForAll "T"
        (TyArrow
          (TyArrow (TyVar "T") (TyVar "U"))
          (TyArrow (TyVar "T") (TyVar "U")))
    tc' [] [] expr3 `shouldBeRight`
      TyForAll "T"
        (TyArrow
          (TyArrow (TyVar "U") (TyVar "T"))
          (TyArrow (TyVar "U") (TyVar "T")))
    tc' [] [] expr4`shouldBeRight` TyForAll "T" (TyArrow (TyVar "T") (TyVar "T"))

  it "typechecks polymorphic type application" $ do
    let tyApp inner = TyApp inner (TyVar "Y")
        expr = tyApp $ VarAnn "x" (TyForAll "X" (TyVar "X"))
        expr2 = Abs "x" (TyForAll "X" (TyVar "X")) (TyApp (Var "x") (TyVar "Y"))
    
    tc' [] [] expr `shouldBeRight` TyVar "Y"
    tc' [] [] expr2 `shouldBeRight` TyForAll "X" (TyArrow (TyVar "X") (TyVar "Y"))
