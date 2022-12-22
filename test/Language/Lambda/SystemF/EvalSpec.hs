module Language.Lambda.SystemF.EvalSpec (spec) where

import RIO
import Test.Hspec

import Language.Lambda.Shared.UniqueSupply (defaultUniques, defaultTyUniques)
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.Eval
import Language.Lambda.SystemF.HspecUtils
import Language.Lambda.SystemF.State

spec :: Spec
spec = do
  describe "evalExpr" $ do
    let evalExpr' expr = execTypecheck (evalExpr expr) $
          mkTypecheckState defaultUniques defaultTyUniques
  
    it "Does not reduce normal form" $ do
      evalExpr' (Var "x") `shouldBeRight` Var "x"

    it "beta reduces" $ do
      evalExpr' (App (Abs "x" (TyVar "T") (Var "x")) (Var "y"))
        `shouldBeRight` Var "y"

    it "reduces multiple applications" pending
  
    it "reduces inner redexes" pending

    it "reduces with name captures" pending
  
    it "reduces let bodies" pending

    it "let expressions update state" pending
  
    it "nested let expressions fail" pending

  describe "subGlobals" $ pure ()

  describe "betaReduce" $ do
    it "reduces simple applications" pending

    it "reduces nested abstractions" pending

    it "reduces inner applications" pending

    it "does not reduce unreducible expressions" pending

    it "does not reduce irreducible chained applications" pending

    it "does not sub shadowed bindings" pending

  describe "alphaConvert" $ pure ()

  describe "etaConvert" $ pure ()

  describe "freeVarsOf" $ pure ()
