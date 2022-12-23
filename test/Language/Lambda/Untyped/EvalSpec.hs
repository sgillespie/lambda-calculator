{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Language.Lambda.Untyped.EvalSpec where

import Data.Map (fromList)
import RIO
import Test.Hspec

import Language.Lambda.Shared.Errors
import Language.Lambda.Untyped

spec :: Spec
spec = do
  describe "evalExpr" $ do
    let evalExpr' expr = execEval (evalExpr expr) (mkEvalState defaultUniques)

    it "beta reduces" $ do
      let expr = App (Abs "x" (Var "x")) (Var "z")
      evalExpr' expr `shouldBe` Right (Var "z")

    it "reduces multiple applications" $ do
      let expr = App (App (Abs "f" (Abs "x" (App (Var "f") (Var "x")))) (Var "g")) (Var "y")
      evalExpr' expr `shouldBe` Right (App (Var "g") (Var "y"))

    it "reduces inner redexes" $ do
      let expr = Abs "x" (App (Abs "y" (Var "y")) (Var "x"))
      evalExpr' expr `shouldBe` Right (Abs "x" (Var "x"))

    it "reduces with name captures" $ do
      let expr = App (Abs "f" (Abs "x" (App (Var "f") (Var "x"))))
                     (Abs "f" (Var "x"))
      evalExpr' expr `shouldBe` Right (Abs "z" (Var "x"))

    it "reduces let bodies" $ do
      let expr = Let "x" $ App (Abs "y" (Var "y")) (Var "z")
      evalExpr' expr `shouldBe` Right (Let "x" (Var "z"))

    it "let expressions update state" $ do
      let res = flip unsafeExecEval (mkEvalState defaultUniques) $ do
            _ <- evalExpr $ Let "w" (Var "x")
            evalExpr $ Var "w"

      res `shouldBe` Var "x"

    it "nested let expressions fail" $ do
      let res = flip unsafeExecEval (mkEvalState defaultUniques) $ do
            evalExpr $ Let "x" (Let "y" (Var "z"))
      evaluate res `shouldThrow` isLetError

  describe "subGlobals" $ do
    let globals' :: Map String (LambdaExpr String)
        globals' = fromList [("w", Var "x")] 
        subGlobals' = subGlobals globals'
    
    it "subs simple variables" $
      subGlobals' (Var "w") `shouldBe` Var "x"

    it "does not sub shadowed bindings" $ do
      let expr = Abs "w" (Var "w")
      subGlobals' expr `shouldBe` expr

    xit "does not capture globals" $ do
      let expr = Abs "x" (Var "w")
      subGlobals' expr `shouldBe` Abs "a" (Var "x")

  describe "betaReduce" $ do
    let betaReduce' :: LambdaExpr Text -> LambdaExpr Text -> LambdaExpr Text
        betaReduce' e1 e2 = unsafeExecEval (betaReduce e1 e2) (mkEvalState [])
    
    it "reduces simple applications" $ do
      let e1 = Abs "x" (Var "x")
          e2 = Var "y"
      betaReduce' e1 e2 `shouldBe` Var "y"

    it "reduces nested abstractions" $ do
      let e1 = Abs "x" (Abs "y" (Var "x"))
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` Abs "y" (Var "z")

    it "reduces inner applications" $ do
      let e1 = Abs "f" (App (Var "f") (Var "x"))
          e2 = Var "g"
      betaReduce' e1 e2 `shouldBe` App (Var "g") (Var "x")

    it "does not reduce unreducible expression" $ do
      let e1 = Var "x"
          e2 = Var "y"
      betaReduce' e1 e2 `shouldBe` App (Var "x") (Var "y")

    it "does not reduce irreducible chained applications" $ do
      let e1 = App (Var "x") (Var "y")
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

    it "does not sub shadowed bindings" $ do
      let e1 = Abs "x" (Abs "x" (Var "x"))
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` Abs "x" (Var "x")

    it "avoids captures" $ do
      let beta :: LambdaExpr Text -> LambdaExpr Text -> LambdaExpr Text
          beta e1 e2 = unsafeExecEval (betaReduce e1 e2) (mkEvalState ["z"])
      
      let e1 = Abs "f" $ Abs "x" $ App (Var "f") (Var "x")
          e2 = Abs "f" $ Var "x"
      beta e1 e2 `shouldBe` Abs "z" (Var "x")

  describe "alphaConvert" $ do
    let alphaConvert' :: [Text] -> [Text] -> LambdaExpr Text -> LambdaExpr Text
        alphaConvert' uniques' fvs expr
          = unsafeExecEval (alphaConvert fvs expr) (mkEvalState uniques')
    
    it "alpha converts simple expressions" $ do
      let freeVars = ["x"] :: [Text]
          expr = Abs "x" (Var "x")
          uniques' = ["y"]
      alphaConvert' uniques' freeVars expr `shouldBe` Abs "y" (Var "y")
  
    it "avoids captures" $ do
      let freeVars = ["x"]
          expr = Abs "x" (Var "x")
          uniques' = ["x", "y"]
      alphaConvert' uniques' freeVars expr `shouldBe` Abs "y" (Var "y")

  describe "etaConvert" $ do
    it "eta converts simple expressions" $ do
      let expr :: LambdaExpr Text
          expr = Abs "x" $ App (Var "f") (Var "x") :: LambdaExpr Text
      etaConvert expr `shouldBe` Var "f" 

    it "eta converts nested applications" $ do
      let expr :: LambdaExpr Text
          expr = Abs "y" $ App (App (Var "f") (Var "x")) (Var "y")
      etaConvert expr `shouldBe` App (Var "f") (Var "x")

      let expr' :: LambdaExpr Text
          expr' = Abs "x" $ Abs "y" (App (App (Var "f") (Var "x")) (Var "y"))
      etaConvert expr' `shouldBe` Var "f" 

      let expr'' :: LambdaExpr Text
          expr'' = Abs "x" (Abs "y" (App (Var "y") (Var "x")))
      etaConvert expr'' `shouldBe` expr''

      let expr''' :: LambdaExpr Text
          expr''' = Abs "f" (Abs "x" (Var "x"))
      etaConvert expr''' `shouldBe` expr'''

    it "ignores non-eta convertable expressions" $ do
      let expr :: LambdaExpr Text
          expr = Abs "x" $ Var "x"
      etaConvert expr `shouldBe` expr

  describe "freeVarsOf" $ do
    let freeVarsOf' :: LambdaExpr Text -> [Text]
        freeVarsOf' = freeVarsOf
    
    it "Returns simple vars" $
      freeVarsOf' (Var "x") `shouldBe` ["x"]
  
    it "Does not return bound vars" $
      freeVarsOf' (Abs "x" (Var "x")) `shouldBe` []

    it "Returns nested simple vars" $
      freeVarsOf' (Abs "x" (Var "y")) `shouldBe` ["y"]

    it "Returns applied simple vars" $
      freeVarsOf' (App (Var "x") (Var "y")) `shouldBe` ["x", "y"]
