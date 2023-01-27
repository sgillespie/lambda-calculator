module Language.Lambda.SystemF.TypeCheckSpec (spec) where

import Language.Lambda.Shared.Errors
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.State
import Language.Lambda.SystemF.HspecUtils

import Control.Monad.Except
import Data.Map
import Lens.Micro
import RIO
import Test.Hspec

spec :: Spec
spec = describe "typecheck" $ do
  let someGlobal = ("x", TypedExpr (Var "y") (TyVar "X"))
  
  it "typechecks simple variables" $ do
    typecheck' [someGlobal] "x" `shouldHaveType` "X"
    typecheck' [] "x" `shouldHaveType` "Z"

  it "typechecks annotated variables" $ do
    typecheck' [someGlobal] "x:X" `shouldHaveType` "X"
    typecheck' [someGlobal] "x:X" `shouldHaveType` "X"

    typecheck' [someGlobal] "x:Y" `shouldFailWith` isTyMismatchError

  it "typechecks abstractions" $
    typecheck' [] "\\x:A. x" `shouldHaveType` "A -> A"

  it "typechecks applications" $ do
    let globals'
          = [ ("f", TypedExpr (Var "f") $ TyArrow (TyVar "T") (TyVar "U")),
              ("a", TypedExpr (Var "a") $ TyVar "T"),
              ("b", TypedExpr (Var "b") (TyVar "B"))
            ]

    typecheck' globals' "f a" `shouldHaveType` "U"
    typecheck' [] "(\\t: T. t) x:T" `shouldHaveType` "T"

    -- Polymorphic application
    typecheck' [] "\\x:(forall T. T). x"
      `shouldHaveType` "forall T. T -> T"
    typecheck' [] "\\x:(forall T. T->U). x"
      `shouldHaveType` "forall T. (T -> U) -> (T -> U)"
    typecheck' [] "\\x:(U->(forall T. T)). x"
      `shouldHaveType` "forall T. (U -> T) -> (U -> T)"
    typecheck' [] "\\x:(forall T. T). x:T"
      `shouldHaveType` "forall T. T -> T"

    typecheck' globals' "a b" `shouldFailWith` isTyMismatchError
    typecheck' globals' "f b" `shouldFailWith` isTyMismatchError

  it "typechecks let expressions" $ do
    typecheck' [] "let x = y" `shouldHaveType` "Z"
    typecheck' [] "\\x:T. let y = z" `shouldFailWith` isLambdaException

  it "typechecks type abstractions" $ do
    typecheck' [] "\\X. (\\x:X. x)" `shouldHaveType` "forall X. X->X"
    typecheck' [] "\\X. x" `shouldHaveType` "forall X. Z"

  it "typechecks type applications" $ do
    let globals'
          = [ ("y", TypedExpr (Var "y") (TyVar "Y")),
              ("x", TypedExpr (Var "x") $ TyVar "A")]

    typecheck' globals' "((\\X.\\x:X.x) [Y]) y" `shouldHaveType` "Y"
    typecheck' globals' "(\\X. x) [T]" `shouldHaveType` "A"
    typecheck' globals' "(\\X. z: X) [T]" `shouldHaveType` "T"
    typecheck' globals' "(\\X. (\\x:X. x)) [Y]" `shouldHaveType` "Y -> Y"
    typecheck' globals' "(z:forall X. X) [Y]" `shouldHaveType` "Y"
    typecheck' globals' "\\x:(forall X. X). x [Y]" `shouldHaveType` "forall X. X -> Y"

    typecheck' [] "x:T [U]" `shouldFailWith` isTyMismatchError
    typecheck' globals' "x [U]" `shouldFailWith` isTyMismatchError

  it "doesn't modify context" $ do
    let exprs
          = [ "\\x:A. x",
              "\\X. x" ]

    forM_ exprs $ \expr -> do
      let ctx = do
            (_, state) <- runTypecheck' [] expr
            pure $ state ^. _context
      
      ctx `shouldBeRight` empty
