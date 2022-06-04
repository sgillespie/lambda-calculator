module Language.Lambda.SystemF.Parser (
  parseExpr,
  parseType
  ) where

import Control.Monad
import Data.Functor
import RIO hiding ((<|>), abs, many, try)
import qualified RIO.Text as Text

import Text.Parsec
import Text.Parsec.Text

import Language.Lambda.SystemF.Expression

parseExpr :: Text -> Either ParseError (SystemFExpr Text Text)
parseExpr = parse (whitespace *> expr <* eof) ""

parseType :: Text -> Either ParseError (Ty Text)
parseType = parse (whitespace *> ty <* eof) ""

-- Parse expressions
expr :: Parser (SystemFExpr Text Text)
expr = try tyapp <|> try app <|> term

app :: Parser (SystemFExpr Text Text)
app = chainl1 term (return App)

tyapp :: Parser (SystemFExpr Text Text)
tyapp = TyApp
      <$> term
      <*> ty'
  where ty' = symbol '[' *> ty <* symbol ']'

term :: Parser (SystemFExpr Text Text)
term = try abs <|> tyabs <|> var <|> parens expr

var :: Parser (SystemFExpr Text Text)
var = Var <$> exprId

abs :: Parser (SystemFExpr Text Text)
abs = curry'
    <$> (symbol '\\' *> many1 args <* symbol '.') 
    <*> expr
  where args = (,) <$> (exprId <* symbol ':') <*> ty
        curry' = flip . foldr . uncurry $ Abs

tyabs :: Parser (SystemFExpr Text Text)
tyabs = curry' <$> args <*> expr
  where args = symbol '\\' *> many1 typeId <* symbol '.'
        curry' = flip (foldr TyAbs)

-- Parse type expressions
ty :: Parser (Ty Text)
ty = try arrow

arrow :: Parser (Ty Text)
arrow = chainr1 tyterm (symbol' "->" $> TyArrow)

tyterm :: Parser (Ty Text)
tyterm = tyvar <|> parens ty

tyvar :: Parser (Ty Text)
tyvar = TyVar <$> typeId

parens :: Parser a -> Parser a
parens p = symbol '(' *> p <* symbol ')'

identifier :: Parser Char -> Parser Text
identifier firstChar = lexeme $ Text.cons <$> first' <*> (Text.pack <$> many rest)
  where first' = firstChar <|> char '_'
        rest = first' <|> digit

typeId, exprId :: Parser Text
typeId = identifier upper
exprId = identifier lower

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

symbol :: Char -> Parser ()
symbol = void . lexeme . char

symbol' :: Text -> Parser ()
symbol' = void . lexeme . string . Text.unpack

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
