module Language.Lambda.Untyped.Parser
  ( parseExpr,
    module Text.Parsec
  ) where

import Control.Monad
import RIO hiding ((<|>), abs, curry, many, try)
import qualified RIO.Text as Text

import Text.Parsec
import Text.Parsec.Text

import Language.Lambda.Untyped.Expression

parseExpr :: Text -> Either ParseError (LambdaExpr Text)
parseExpr = parse (whitespace *> expr <* eof) ""

expr :: Parser (LambdaExpr Text)
expr = try app <|> term

term :: Parser (LambdaExpr Text)
term = let' <|> abs <|> var <|> parens

var :: Parser (LambdaExpr Text)
var = Var <$> identifier

abs :: Parser (LambdaExpr Text)
abs = curry <$> idents <*> expr
  where idents = symbol '\\' *> many1 identifier <* symbol '.'
        curry = flip (foldr Abs)

app :: Parser (LambdaExpr Text)
app = chainl1 term (return App)

let' :: Parser (LambdaExpr Text)
let' = Let <$> ident <*> expr
  where ident = keyword "let" *> identifier <* symbol '='

parens :: Parser (LambdaExpr Text)
parens = symbol '(' *> expr <* symbol ')'

lexeme :: Parser a -> Parser a
lexeme p =  p <* whitespace

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

identifier :: Parser Text
identifier = lexeme $ Text.cons <$> first' <*> (Text.pack <$> many rest)
  where first' = letter <|> char '_'
        rest  = first' <|> digit

symbol :: Char -> Parser ()
symbol = void . lexeme . char

keyword :: Text -> Parser ()
keyword = void . lexeme . string . Text.unpack
