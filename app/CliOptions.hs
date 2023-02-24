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
  <$> language
  <*> switch version
  where language = flag' SystemF systemF <|> flag Untyped Untyped untyped

        version = long "version"
          <> short 'v'
          <> help "Print the version"

        systemF = long "system-f"
          <> short 'f'
          <> help "Use the System F interpreter"

        untyped = long "untyped"
          <> short 'l'
          <> help "Use the Untyped Lambda Calculus interpreter"
