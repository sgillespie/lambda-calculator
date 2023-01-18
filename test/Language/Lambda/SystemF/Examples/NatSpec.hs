module Language.Lambda.SystemF.Examples.NatSpec where

import RIO
import Test.Hspec

import Language.Lambda.SystemF (evalText)
import Language.Lambda.SystemF.HspecUtils

spec :: Spec
spec = describe "Nat" $ do
  -- Nat is the definition of natural numbers. More precisely, Nat
  -- is the set of nonnegative integers.  We represent nats using
  -- Church Encodings:
  --
  -- 0: \f:(T->T) x:T. x
  -- 1: \f:(T->T) x:T. f x
  -- 2: \f:(T->T) x:T. f (f x)
  -- ...and so on

  describe "successor" $ do
    -- successor is a function that adds 1
    -- succ(0) = 1
    -- succ(1) = 2
    -- ... and so forth
    --
    -- successor is defined by
    -- succ = \n:((T->T)->T->T) f:(T->T) x:T. f (n f x)
    it "succ 0 = 1" $ do
      "(\\n:((T->T)->T->T) f:(T->T) x:T. f (n f x)) (\\f:(T->T) x:T. x)"
        `shouldEvalTo` "\\f:(T->T) x:T. f x"

    it "succ 1 = 2" $
      "(\\n:((T->T)->T->T) f:(T->T) x:T. f (n f x)) (\\f:(T->T) x:T. f x)"
        `shouldEvalTo` "\\f:(T->T) x:T. f (f x)"

  describe "add" $ do
    -- add(m, n) = m + n
    --
    -- It is defined by applying successor m times on n:
    -- add = \m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T) x:T. m f (n f x)
    it "add 0 2 = 2" $
      "(\\m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T) x:T. m f (n f x)) (\\f:(T->T) x:T. x) (\\f:(T->T) x:T. f (f x))"
        `shouldEvalTo` "\\f:(T->T) x:T. f (f x)"

    it "add 3 2 = 5" $
      "(\\m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T) x:T. m f (n f x)) (\\f:(T->T) x:T. f (f (f x))) (\\f:(T->T) x:T. f (f x))"
        `shouldEvalTo` "\\f:(T->T) x:T. f (f (f (f (f x))))"

    it "add 0 n = n" $
      "(\\m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T) x:T. m f (n f x)) (\\f:(T->T) x:T. x) n:((T->T)->T->T)"
        `shouldEvalTo` "\\f:(T->T) x:T. n:((T->T)->T->T) f x"

  describe "multiply" $ do
    -- multiply(m, n) = m * n
    --
    -- multiply is defined by applying add m times
    -- multiply = \m n f x. m (n f x) x)
    --
    -- Using eta conversion, we can omit the parameter x
    -- multiply = \m n f. m (n f)
    it "multiply 0 2 = 0" $
      "(\\m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T). m (n f)) (\\f:(T->T) x:T. x) (\\f:(T->T) x:T. f (f x))"
        `shouldEvalTo` "\\f:(T->T) x:T. x"

    it "multiply 2 3 = 6" $
      "(\\m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T). m (n f)) (\\f:(T->T) x:T. f (f x)) (\\f:(T->T) x:T. f (f (f x)))"
        `shouldEvalTo` "\\f:(T->T) x:T. f (f (f (f (f (f x)))))"

    it "multiply 0 n = 0" $
      "(\\m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T). m (n f)) (\\f:(T->T) x:T. x) n:((T->T)->T->T)"
        `shouldEvalTo` "\\f:(T->T) x:T. x"

    it "multiply 1 n = n" $
      "(\\m:((T->T)->T->T) n:((T->T)->T->T) f:(T->T). m (n f)) (\\f:(T->T) x:T. f x) n:((T->T)->T->T)"
        `shouldEvalTo` "\\f:(T->T) x:T. n:((T->T)->T->T) f x"
