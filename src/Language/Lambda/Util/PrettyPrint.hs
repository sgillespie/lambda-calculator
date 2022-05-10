{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda.Util.PrettyPrint where

import qualified Data.List as L

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint String where
  prettyPrint = id
  
newtype PDoc s = PDoc [s]
  deriving (Eq, Show)

instance PrettyPrint s => PrettyPrint (PDoc s) where
  prettyPrint (PDoc ls) = concatMap prettyPrint ls

instance Semigroup (PDoc s) where
  (PDoc p1) <> (PDoc p2) = PDoc $ p1 ++ p2

instance Monoid (PDoc s) where
  mempty = empty

instance Functor PDoc where
  fmap f (PDoc ls) = PDoc (fmap f ls)

empty :: PDoc s
empty = PDoc []

add :: s -> PDoc s -> PDoc s
add s (PDoc ps) = PDoc (s:ps)

append :: [s] -> PDoc s -> PDoc s
append =  mappend . PDoc

between :: PDoc s -> s -> s -> PDoc s -> PDoc s
between (PDoc str) start end pdoc = PDoc ((start:str) ++ [end]) `mappend` pdoc

betweenParens :: PDoc String -> PDoc String -> PDoc String
betweenParens doc = between doc "(" ")"

intercalate :: [[s]] -> [s] -> PDoc [s] -> PDoc [s]
intercalate ss sep = add $ L.intercalate sep ss

addSpace :: PDoc String -> PDoc String
addSpace = add [space]
  
space :: Char
space = ' '

lambda :: Char
lambda = 'λ'

upperLambda :: Char
upperLambda = 'Λ'
