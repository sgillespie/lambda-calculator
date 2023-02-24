module Language.Lambda.SystemF.Examples.BoolSpec where

import RIO
import Test.Hspec

import Language.Lambda.SystemF.HspecUtils

spec :: Spec
spec = describe "Bool" $ do
  -- Bool is the definition of Booleans. We represent bools
  -- using Church Encodings:
  --
  -- true:  \T. \t:T f:T. t
  -- false: \T. \t:T f:T. f
  -- false: \T. \t:T f:T. f
  describe "not" $ do
    -- not takes a Bool and returns its opposite value
    --
    -- not(true)  = false
    -- not(false) = true
    --
    -- not is defined by
    -- not = \x. x (\t f. f) (\t f. t)
    it "not true = false" $
      "(\\x:(forall T. T -> T -> T). \\X. \\t:X f:X. (x [X]) f t) (\\U. \\t:U f:U. t)"
        `shouldEvalTo` "\\X. \\t:X f:X. f"

    it "not false = true" $
      "(\\x:(forall T. T-> T -> T). \\X. \\t:X f:X. (x [X]) f t) (\\U. \\t:U f:U. f)"
        `shouldEvalTo` "\\X. \\t:X f:X. t"
