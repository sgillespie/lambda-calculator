module Main (main) where

import Language.Haskell.HLint (hlint)
import RIO

arguments :: [String]
arguments = [ 
  "app",
  "src",
  "test"
  ]

main :: IO ()
main = hlint arguments >>= main'
  where main' [] = exitSuccess
        main' _  = exitFailure
