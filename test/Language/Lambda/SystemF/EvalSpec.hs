module Language.Lambda.SystemF.EvalSpec (spec) where

import RIO
import RIO.Map (fromList)
import Test.Hspec

import Language.Lambda.Shared.Errors (isLetError, isImpossibleError)
import Language.Lambda.Shared.UniqueSupply (defaultUniques, defaultTyUniques)
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Eval
import Language.Lambda.SystemF.HspecUtils
import Language.Lambda.SystemF.State

spec :: Spec
spec = do
  let evalExpr' expr = execTypecheck (evalExpr expr) $
          mkTypecheckState defaultUniques defaultTyUniques
  
  describe "evalExpr" $ do
    it "Does not reduce normal form" $ do
      evalExpr' (Var "x") `shouldBeRight` Var "x"

    it "beta reduces" $ do
      evalExpr' (App (Abs "x" (TyVar "T") (Var "x")) (Var "y"))
        `shouldBeRight` Var "y"

    it "reduces multiple applications" $ do
      let expr = App innerL innerR
          innerL = App
            (Abs "f" (TyArrow (TyVar "T") (TyVar "T")) $
              Abs "x" (TyVar "T") $
                App (Var "f") (Var "x"))
            (Var "g")
          innerR = Var "y"

      evalExpr' expr `shouldBeRight` App (Var "g") (Var "y")
  
    it "reduces inner redexes" $ do
      let expr = Abs "x" (TyVar "T") $
            App
              (Abs "y" (TyVar "T") (Var "y"))
              (Var "x")

      evalExpr' expr `shouldBeRight` Abs "x" (TyVar "T") (Var "x")

    it "reduces with name captures" $ do
      let expr = App innerL innerR
          innerL = Abs "f" (TyArrow (TyVar "T") (TyVar "T")) $
            Abs "x" (TyVar "T") $
              App (Var "f") (Var "x")
          innerR = Abs "f" (TyVar "U") (Var "x")

      evalExpr' expr `shouldBeRight` Abs "z" (TyVar "T") (Var "x")
  
    it "reduces let bodies" $ do
      let expr = Let "x" $
            App
              (Abs "y" (TyVar "Y") (Var "y"))
              (Var "z")
      evalExpr' expr `shouldBeRight` Let "x" (Var "z")

    it "nested let expressions fail" $ do
      let res = evalExpr' (Let "x" (Let "y" (Var "z")))
      res `shouldSatisfy` either isLetError (const False)

    it "reduces type abstractions to A normal form" $ do
      let expr = TyAbs "T" $
            App
              (Abs "y" (TyVar "T") (Var "y"))
              (VarAnn "x" (TyVar "T"))

      evalExpr' expr `shouldBeRight` TyAbs "T" (VarAnn "x" (TyVar "T"))

    it "reduces simple type applications" $ do
      let expr = TyApp
            (TyAbs "T" (VarAnn "x" (TyVar "T")))
            (TyVar "X")
      evalExpr' expr `shouldBeRight` VarAnn "x" (TyVar "X")

  describe "subGlobals" $ do
    let subGlobals' :: SystemFExpr Text -> SystemFExpr Text
        subGlobals' expr = unsafeExecTypecheck (subGlobals expr) state
        state = TypecheckState globals' defaultUniques defaultTyUniques
        globals' = fromList [("w", TypedExpr (Var "x") (TyVar "X"))]
    
    it "subs simple variables" $ do
      subGlobals' (Var "w")  `shouldBe` Var "x"
      subGlobals' (VarAnn "w" (TyVar "X"))  `shouldBe` Var "x"
      
    it "does not sub shadowed bindings" $ do
      let expr = Abs "w" (TyVar "W") $ Var "w"
      subGlobals' expr `shouldBe` expr
      
    xit "does not capture globals" $ do
      let expr = Abs "x" (TyVar "X") $ Var "w"
      subGlobals' expr `shouldBe` Abs "a" (TyVar "X") (Var "x")

  describe "betaReduce" $ do
    let betaReduce' :: SystemFExpr Text -> SystemFExpr Text -> SystemFExpr Text
        betaReduce' e1 e2 = unsafeExecTypecheck (betaReduce e1 e2) $
          mkTypecheckState defaultUniques defaultTyUniques
    
    it "reduces simple applications" $ do
      let e1 = Abs "x" (TyVar "T") (Var "x")
          e2 = Var "y"

      betaReduce' e1 e2 `shouldBe` e2

    it "reduces nested abstractions" $ do
      let e1 = Abs "x" (TyVar "T") (Abs "y" (TyVar "U") (Var "x"))
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` Abs "y" (TyVar "U") (Var "z")

    it "reduces inner applications" $ do
      let e1 = Abs "f" (TyArrow (TyVar "T") (TyVar "T")) $
            App (Var "f") (VarAnn "x" (TyVar "T"))
          e2 = Var "g"
      betaReduce' e1 e2 `shouldBe` App (Var "g") (VarAnn "x" (TyVar "T"))

    it "does not reduce unreducible expressions" $ do
      let e2 = Var "y"

      betaReduce' (Var "x") e2
        `shouldBe` App (Var "x") (Var "y")
      betaReduce' (VarAnn "x" (TyVar "T")) e2
        `shouldBe` App (VarAnn "x" (TyVar "T")) (Var "y")
      betaReduce' (TyAbs "X" (Var "x")) e2
        `shouldBe` App (TyAbs "X" (Var "x")) e2
      betaReduce' (TyApp (Var "x") (TyVar "X")) e2
        `shouldBe` App (TyApp (Var "x") (TyVar "X")) e2

    it "does not reduce irreducible chained applications" $ do
      let e1 = App (Var "x") (Var "y")
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

    it "does not sub shadowed bindings" $ do
      let e1 = Abs "x" (TyVar "T") (Abs "x" (TyVar "U") (Var "x"))
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` Abs "x" (TyVar "U") (Var "x")

    it "fails to reduce Let" $ do
      let e1 = Let "x" (Var "x")
          e2 = Var "z"
      evaluate (betaReduce' e1 e2) `shouldThrow` isImpossibleError

    it "avoids capture" $ do
      let beta :: SystemFExpr Text -> SystemFExpr Text -> SystemFExpr Text
          beta e1 e2 = unsafeExecTypecheck (betaReduce e1 e2) $
            mkTypecheckState ["z"] defaultTyUniques
      
      let e1 = Abs "f" (TyArrow (TyVar "T") (TyVar "U")) $
            Abs "x" (TyVar "U") $
              App (Var "f") (Var "x")
          e2 = Abs "f" (TyVar "T") $ Var "x"
      beta e1 e2 `shouldBe` Abs "z" (TyVar "U") (Var "x")

  describe "evalTyApp" $ do
    it "reduces simple type applications" $ do
      let expr = TyApp
            (TyAbs "T" (VarAnn "x" (TyVar "T")))
            (TyVar "X")

      evalExpr' expr `shouldBeRight` VarAnn "x" (TyVar "X")

    it "reduces type applications with abstractions" $ do
      let expr = TyApp
            (TyAbs "T" (Abs "x" (TyVar "T") (Var "x")))
            (TyVar "X")

      evalExpr' expr `shouldBeRight` Abs "x" (TyVar "X") (Var "x")

    it "does not reduce irreducible expressions" $ do
      let tyApp inner = TyApp (TyAbs "T" inner) (TyVar "X")

      evalExpr' (tyApp (Var "x")) `shouldBeRight` Var "x"
      evalExpr' (tyApp (VarAnn "x" (TyVar "Z"))) `shouldBeRight` VarAnn "x" (TyVar "Z")
      evalExpr' (tyApp (Abs "x" (TyVar "Z") (Var "x")))
        `shouldBeRight` Abs "x" (TyVar "Z") (Var "x")

    it "fails on let" $ do
      let expr = TyApp (Let "x" (VarAnn "y" (TyVar "T"))) (TyVar "X")
      evalExpr' expr `shouldSatisfy` either isLetError (const False)

    it "reduces nested expressions" $ do
      let tyApp inner = TyApp (TyAbs "T" inner) (TyVar "X")
      
      let e1 = App (Var "f") (VarAnn "x" (TyVar "T"))
      evalExpr' (tyApp e1) `shouldBeRight` App (Var "f") (VarAnn "x" $ TyVar "X")

      let e2 = Abs "x" (TyVar "U") (VarAnn "t" $ TyVar "T")
      evalExpr' (tyApp e2) `shouldBeRight` Abs "x" (TyVar "U") (VarAnn "t" $ TyVar "X")

      let e3 = TyAbs "U" $ VarAnn "x" (TyVar "T")
      evalExpr' (tyApp e3) `shouldBeRight` TyAbs "U" (VarAnn "x" $ TyVar "X")

      let e4 = TyApp (VarAnn "x" (TyVar "T")) (TyVar "U")
      evalExpr' (tyApp e4) `shouldBeRight` TyApp (VarAnn "x" $ TyVar "X") (TyVar "U")

      let e5 = TyApp
            (TyAbs "U" $ VarAnn "x" (TyVar "U"))
            (TyVar "T")
      evalExpr' (tyApp e5) `shouldBeRight` VarAnn "x" (TyVar "X")

    it "reduces in nested types" $ do
      let tyApp inner = TyApp (TyAbs "T" inner) (TyVar "X")

      let e1 = VarAnn "f" $ TyArrow (TyVar "T") (TyVar "U")
      evalExpr' (tyApp e1) `shouldBeRight` VarAnn "f" (TyArrow (TyVar "X") (TyVar "U"))

      let e2 = VarAnn "f" $ TyForAll "T" (TyVar "T")
      evalExpr' (tyApp e2) `shouldBeRight` e2

      let e3 = VarAnn "f" $ TyForAll "U" (TyVar "T")
      evalExpr' (tyApp e3) `shouldBeRight` VarAnn "f" (TyForAll "U" (TyVar "X"))
                

  describe "alphaConvert" $ do
    let alphaConvert' :: [Text] -> [Text] -> SystemFExpr Text -> SystemFExpr Text
        alphaConvert' uniques' fvs expr = unsafeExecTypecheck (alphaConvert fvs expr) $
          mkTypecheckState uniques' defaultTyUniques
    
    it "alpha converts simple expressions" $ do
      let freeVars = ["x"] :: [Text]
          expr = Abs "x" (TyVar "T") (Var "x")
          uniques' = ["y"]
      alphaConvert' uniques' freeVars expr `shouldBe` Abs "y" (TyVar "T") (Var "y")

    it "avoids captures" $ do
      let freeVars = ["x"]
          expr = Abs "x" (TyVar "T") (Var "x")
          uniques' = ["x", "y"]
      alphaConvert' uniques' freeVars expr `shouldBe` Abs "y" (TyVar "T") (Var "y")

  describe "etaConvert" $ do
    it "eta converts simple expressions" $ do
      let expr :: SystemFExpr Text
          expr = Abs "x" (TyVar "T") $ App (Var "f") (Var "x")
      etaConvert expr `shouldBe` Var "f"
      
    it "eta converts nested applications" $ do
      let expr1 :: SystemFExpr Text
          expr1 = Abs "y" (TyVar "T")  $ App (App (Var "f") (Var "x")) (Var "y")
      etaConvert expr1 `shouldBe` App (Var "f") (Var "x")

      let expr2 :: SystemFExpr Text
          expr2 = Abs "x" (TyArrow (TyVar "T") (TyVar "T")) $
            Abs "y" (TyVar "T") $
              App (App (Var "f") (Var "x")) (Var "y")
      etaConvert expr2 `shouldBe` Var "f" 

      let expr3 :: SystemFExpr Text
          expr3 = Abs "x" (TyVar "T") $
            Abs "y" (TyArrow (TyVar "T") (TyVar "T")) $
              App (Var "y") (Var "x")
      etaConvert expr3 `shouldBe` expr3

      let expr4 :: SystemFExpr Text
          expr4 = Abs "f" (TyVar "T") $
            Abs "x" (TyVar "T") (Var "x")
      etaConvert expr4 `shouldBe` expr4
      
    it "ignores non-eta convertable expressions" $ do
      let expr :: SystemFExpr Text
          expr = Abs "x" (TyVar "T") $ Var "x"
      etaConvert expr `shouldBe` expr

  describe "freeVarsOf" $ do
    let freeVarsOf' :: SystemFExpr Text -> [Text]
        freeVarsOf' = freeVarsOf
    
    it "Returns simple vars" $ do
      freeVarsOf' (Var "x") `shouldBe` ["x"]
      freeVarsOf' (VarAnn "x" (TyVar "T")) `shouldBe` ["x"]
      
    it "Does not return bound vars" $ 
      freeVarsOf' (Abs "x" (TyVar "T") (Var "x")) `shouldBe` []
      
    it "Returns nested simple vars" $
      freeVarsOf' (Abs "x" (TyVar "T") (Var "y")) `shouldBe` ["y"]
                                                             
    it "Returns applied simple vars" $
      freeVarsOf' (App (Var "x") (Var "y")) `shouldBe` ["x", "y"]
