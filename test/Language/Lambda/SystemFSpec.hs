module Language.Lambda.SystemFSpec where

import Language.Lambda.Shared.Errors (LambdaException(..))
import Language.Lambda.SystemF
import Language.Lambda.SystemF.HspecUtils

import Lens.Micro
import RIO
import Test.Hspec

spec :: Spec
spec = describe "evalString" $ do
  let eval' :: Text -> Either LambdaException (SystemFExpr Text)
      eval' = over _Right (^. _expr) . eval

  it "evaluates simple text" $ do
    eval' "x" `shouldBeRight` Var "x"
    eval' "\\x:T. x" `shouldBeRight` Abs "x" (TyVar "T") (Var "x")
    eval' "\\X. x" `shouldBeRight` TyAbs "X" (Var "x")
    eval' "x [T]" `shouldBeRight` TyApp (Var "x") (TyVar "T")
