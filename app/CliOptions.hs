module CliOptions
  ( CliOptions(..),
    Language(..),
    parseCliOptions
  ) where

import RIO

import Options.Applicative hiding (command, ParseError())

data CliOptions = CliOptions {
  language :: Language,
  version :: Bool
}

-- | Supported Languages:
-- 
--    * Untyped Lambda Calculus
--    * System F
data Language 
  = Untyped
  | SystemF

parseCliOptions :: IO CliOptions
parseCliOptions = execParser opts
  where opts = info
          (helper <*> cliParser)
          (briefDesc <> progDesc "A Lambda Calculus Interpreter")

cliParser :: Parser CliOptions
cliParser = CliOptions 
  <$> flag Untyped SystemF language
  <*> switch version
  where language = long "system-f"
          <> short 'f'
          <> internal -- this is a secret feature
          <> help "Use the System F interpreter"

        version = long "version"
          <> short 'v'
          <> help "Print the version"
