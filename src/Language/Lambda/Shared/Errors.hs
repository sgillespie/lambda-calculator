module Language.Lambda.Shared.Errors
  ( LambdaException(..),
    isLambdaException,
    isLetError,
    isParseError,
    isImpossibleError
  ) where

import RIO

data LambdaException
  -- | An expression that cannot be parsed
  -- Examples:
  --
  --     \x y
  --     = y
  = ParseError Text

  -- | A let binding nested in another expression
  -- Examples:
  --
  --     \x. let y = z
  --     x (let y = z)
  | InvalidLet Text -- ^ A let binding nested in another expression

  -- | The expected type does not match the actual type
  -- Examples:
  --
  --     (\x: X. x) (y:Y)
  | TyMismatchError Text

  -- | A catch-all error that indicates a bug in this project
  | ImpossibleError
  deriving (Eq, Typeable)

instance Exception LambdaException

instance Display LambdaException where
  textDisplay (ParseError txt) = "Parse error " <> txt
  textDisplay (InvalidLet txt) = "Illegal nested let: " <> txt
  textDisplay ImpossibleError = "An impossible error occurred! Please file a bug."

instance Show LambdaException where
  show = show . textDisplay

-- | Returns true if the passed in value is a LamdbaExpression. Can be used, for example,
-- as a `shouldThrow` matcher
isLambdaException :: LambdaException -> Bool
isLambdaException _ = True

isLetError :: LambdaException -> Bool
isLetError (InvalidLet _) = True
isLetError _ = False

isParseError :: LambdaException -> Bool
isParseError (ParseError _) = True
isParseError _ = False

isImpossibleError :: LambdaException -> Bool
isImpossibleError ImpossibleError = True
isImpossibleError _ = False
