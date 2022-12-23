module Language.Lambda.Shared.UniqueSupply where

import Language.Lambda.Shared.Errors (LambdaException(..))

import Control.Monad.Except (MonadError(..), throwError)
import RIO
import RIO.List (find)
import RIO.Text (pack, toUpper)

type Unique = Text

defaultUniques :: [Unique]
defaultUniques = map pack strings
  where strings = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
        suffix = "" : map show [(0::Int)..]

defaultTyUniques :: [Unique]
defaultTyUniques = map toUpper defaultUniques

next
  :: (Ord name, MonadError LambdaException m)
  => [name] -- ^ Unique supply
  -> [name] -- ^ Free Variables
  -> m name
next freeVars uniques' = case find (`notElem` freeVars) uniques' of
  Just unique -> pure unique
  Nothing -> throwError ImpossibleError
