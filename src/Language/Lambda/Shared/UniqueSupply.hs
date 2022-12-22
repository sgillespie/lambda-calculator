module Language.Lambda.Shared.UniqueSupply where

import RIO
import RIO.Text (pack, toUpper)

defaultUniques :: [Text]
defaultUniques = map pack strings
  where strings = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
        suffix = "" : map show [(0::Int)..]

defaultTyUniques :: [Text]
defaultTyUniques = map toUpper defaultUniques
