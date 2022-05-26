module Language.Lambda.Untyped.Parser
  ( parseExpr,
    module Text.Parsec
  ) where

import Control.Monad
import Prelude hiding (abs, curry)

import Text.Parsec
import Text.Parsec.String

import Language.Lambda.Untyped.Expression

parseExpr :: String -> Either ParseError (LambdaExpr String)
parseExpr = parse (whitespace *> expr <* eof) ""

expr :: Parser (LambdaExpr String)
expr = try app <|> term

term :: Parser (LambdaExpr String)
term = let' <|> abs <|> var <|> parens

var :: Parser (LambdaExpr String)
var = Var <$> identifier

abs :: Parser (LambdaExpr String)
abs = curry <$> idents <*> expr
  where idents = symbol '\\' *> many1 identifier <* symbol '.'
        curry = flip (foldr Abs)

app :: Parser (LambdaExpr String)
app = chainl1 term (return App)

let' :: Parser (LambdaExpr String)
let' = Let <$> ident <*> expr
  where ident = keyword "let" *> identifier <* symbol '='

parens :: Parser (LambdaExpr String)
parens = symbol '(' *> expr <* symbol ')'

lexeme :: Parser a -> Parser a
lexeme p =  p <* whitespace

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many rest)
  where first = letter <|> char '_'
        rest  = first <|> digit

symbol :: Char -> Parser ()
symbol = void . lexeme . char

keyword :: String -> Parser ()
keyword = void . lexeme . string
