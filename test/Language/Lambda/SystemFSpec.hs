module Language.Lambda.SystemFSpec where

import Language.Lambda.Shared.Errors (LambdaException(..), isLambdaException)
import Language.Lambda.SystemF
import Language.Lambda.SystemF.HspecUtils

import Lens.Micro
import RIO
import RIO.Map (empty)
import Test.Hspec

spec :: Spec
spec = do
  describe "evalString" $ do
    let eval' :: Text -> Either LambdaException (SystemFExpr Text)
        eval' = over _Right (^. _expr) . eval

    it "evaluates simple text" $ do
      eval' "x" `shouldBeRight` Var "x"
      eval' "\\x:T. x" `shouldBeRight` Abs "x" (TyVar "T") (Var "x")
      eval' "\\X. x" `shouldBeRight` TyAbs "X" (Var "x")
      eval' "x [T]" `shouldBeRight` TyApp (Var "x") (TyVar "T")

  describe "runEvalText" $ do
    let runEvalText' input = extract $ runEvalText input empty
        extract = _Right %~ (^. _expr) . fst
    
    it "evaluates simple text" $ do
      runEvalText' "x" `shouldBeRight` Var "x"
      runEvalText' "\\x:T. x" `shouldBeRight` Abs "x" (TyVar "T") (Var "x")
      runEvalText' "\\X. x" `shouldBeRight` TyAbs "X" (Var "x")
      runEvalText' "x [T]" `shouldBeRight` TyApp (Var "x") (TyVar "T")

  describe "execEvalText" $ do
    let execEvalText' input = extract $ execEvalText input empty
        extract = over _Right (^. _expr)
    
    it "evaluates simple text" $ do
      execEvalText' "x" `shouldBeRight` Var "x"
      execEvalText' "\\x:T. x" `shouldBeRight` Abs "x" (TyVar "T") (Var "x")
      execEvalText' "\\X. x" `shouldBeRight` TyAbs "X" (Var "x")
      execEvalText' "x [T]" `shouldBeRight` TyApp (Var "x") (TyVar "T")

  describe "unsafeExecEvalText" $ do
    let unsafeExecEvalText' input = extract $ unsafeExecEvalText input empty
        extract = (^. _expr)

    it "evaluates simple text" $ do
      unsafeExecEvalText' "x" `shouldBe` Var "x"
      unsafeExecEvalText' "\\x:T. x" `shouldBe` Abs "x" (TyVar "T") (Var "x")
      unsafeExecEvalText' "\\X. x" `shouldBe` TyAbs "X" (Var "x")
      unsafeExecEvalText' "x [T]" `shouldBe` TyApp (Var "x") (TyVar "T")

    it "throws errors" $ do
      evaluate (unsafeExecEvalText' "\\x. x") `shouldThrow` isLambdaException
