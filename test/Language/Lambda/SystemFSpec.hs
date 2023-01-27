module Language.Lambda.SystemFSpec where

import Language.Lambda.Shared.Errors (LambdaException(..), isLambdaException)
import Language.Lambda.SystemF
import Language.Lambda.SystemF.HspecUtils

import Lens.Micro
import RIO
import RIO.Map (empty, fromList)
import Test.Hspec

spec :: Spec
spec = do
  describe "evalText" $ do
    let eval' :: Text -> Either LambdaException (SystemFExpr Text)
        eval' = over _Right (^. _expr) . eval

    it "evaluates simple text" $ do
      eval' "x" `shouldBeRight` Var "x"
      eval' "\\x:T. x" `shouldBeRight` Abs "x" (TyVar "T") (Var "x")
      eval' "\\X. x" `shouldBeRight` TyAbs "X" (Var "x")

    it "reduces simple applications" $
      eval' "(\\x:T. x) y:T" `shouldBeRight` VarAnn "y" (TyVar "T")

    it "reduces applications with nested redexes" $
      eval' "(\\f:T->T x:T. f x) (\\y:T. y)"
        `shouldBeRight` Abs "x" (TyVar "T") (Var "x")

    it "lets update state" $ do
      let act = evalText "let x = a: A" >> evalText "x"

      unsafeExecTypecheck act (mkTypecheckState [] [])
        `shouldBe` TypedExpr (VarAnn "a" (TyVar "A")) (TyVar "A")

  describe "runEvalText" $ do
    let runEvalText' input = extract $ runEvalText input empty
        extract = _Right %~ (^. _expr) . fst
    
    it "evaluates simple text" $ do
      runEvalText' "x" `shouldBeRight` Var "x"
      runEvalText' "\\x:T. x" `shouldBeRight` Abs "x" (TyVar "T") (Var "x")
      runEvalText' "\\X. x" `shouldBeRight` TyAbs "X" (Var "x")

  describe "execEvalText" $ do
    let execEvalText' input = extract $ execEvalText input empty
        extract = over _Right (^. _expr)
    
    it "evaluates simple text" $ do
      execEvalText' "x" `shouldBeRight` Var "x"
      execEvalText' "\\x:T. x" `shouldBeRight` Abs "x" (TyVar "T") (Var "x")
      execEvalText' "\\X. x" `shouldBeRight` TyAbs "X" (Var "x")

  describe "unsafeExecEvalText" $ do
    let unsafeExecEvalText' input = extract $ unsafeExecEvalText input empty
        extract = (^. _expr)

    it "evaluates simple text" $ do
      unsafeExecEvalText' "x" `shouldBe` Var "x"
      unsafeExecEvalText' "\\x:T. x" `shouldBe` Abs "x" (TyVar "T") (Var "x")
      unsafeExecEvalText' "\\X. x" `shouldBe` TyAbs "X" (Var "x")

    it "throws errors" $ do
      evaluate (unsafeExecEvalText' "\\x. x") `shouldThrow` isLambdaException

  describe "typecheckText" $ do
    let tc :: Text -> Either LambdaException (Ty Text)
        tc input = execTypecheck (typecheckText input) initialState

        initialState = mkTypecheckState defaultUniques defaultTyUniques

    it "typechecks simple text" $ do
      tc "x" `shouldHaveType` "Z"
      tc "\\x:T. x" `shouldHaveType` "T -> T"
      tc "\\X. x" `shouldHaveType` "forall X. Z"
      tc "(\\x:T. x) y:T" `shouldHaveType` "T"
      tc "(\\f:(T->T) x:T. f x) (\\y:T. y)" `shouldHaveType` "T -> T"

  describe "runTypecheckText" $ do
    let tc :: Text -> Either LambdaException (Ty Text)
        tc input = fst <$> runTypecheckText input globals'

        globals' = fromList [("x", TypedExpr (Var "x") (TyVar "A"))]

    it "typechecks simple text" $ do
      tc "x" `shouldHaveType` "A"
      tc "\\x:T. x" `shouldHaveType` "T -> T"
      tc "\\X. x" `shouldHaveType` "forall X. A"
      tc "(\\x:T. x) y:T" `shouldHaveType` "T"
      tc "(\\f:(T->T) x:T. f x) (\\y:T. y)" `shouldHaveType` "T -> T"

  describe "execTypecheckText" $ do
    let tc :: Text -> Either LambdaException (Ty Text)
        tc input = execTypecheckText input globals'

        globals' = fromList [("x", TypedExpr (Var "x") (TyVar "A"))]

    it "typechecks simple text" $ do
      tc "x" `shouldHaveType` "A"
      tc "\\x:T. x" `shouldHaveType` "T -> T"
      tc "\\X. x" `shouldHaveType` "forall X. A"
      tc "(\\x:T. x) y:T" `shouldHaveType` "T"
      tc "(\\f:(T->T) x:T. f x) (\\y:T. y)" `shouldHaveType` "T -> T"

  describe "unsafeExecTypecheckText" $ do
    let tc :: Text -> Ty Text
        tc input = unsafeExecTypecheckText input globals'

        globals' = fromList [("x", TypedExpr (Var "x") (TyVar "A"))]

    it "typechecks simple text" $ do
      Right (tc "x") `shouldHaveType` "A"
      Right (tc "\\x:T. x") `shouldHaveType` "T -> T"
      Right (tc "\\X. x") `shouldHaveType` "forall X. A"
      Right (tc "(\\x:T. x) y:T") `shouldHaveType` "T"
      Right (tc "(\\f:(T->T) x:T. f x) (\\y:T. y)") `shouldHaveType` "T -> T"
