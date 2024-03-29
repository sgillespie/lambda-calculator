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

parseExpr :: Text -> Either ParseError (SystemFExpr Text)
parseExpr = parse (whitespace *> topLevelExpr <* eof) ""

parseType :: Text -> Either ParseError (Ty Text)
parseType = parse (whitespace *> ty <* eof) ""

-- Lets can only be at the top level
topLevelExpr :: Parser (SystemFExpr Text)
topLevelExpr = let' <|> expr

-- Parse expressions
expr :: Parser (SystemFExpr Text)
expr = try tyapp <|> try app <|> term

app :: Parser (SystemFExpr Text)
app = chainl1 term (return App)

tyapp :: Parser (SystemFExpr Text)
tyapp = TyApp
      <$> term
      <*> ty'
  where ty' = symbol '[' *> ty <* symbol ']'

term :: Parser (SystemFExpr Text)
term = try abs <|> tyabs <|> var <|> parens expr

let' :: Parser (SystemFExpr Text)
let' = Let <$> ident <*> expr
  where ident = symbol' "let" *> exprId <* symbol '='

var :: Parser (SystemFExpr Text)
var = try varann <|> var'
  where var' = Var <$> exprId
        varann = VarAnn <$> (exprId <* symbol ':') <*> ty

abs :: Parser (SystemFExpr Text)
abs = curry'
    <$> (symbol '\\' *> many1 args <* symbol '.')
    <*> expr
  where args = (,) <$> (exprId <* symbol ':') <*> ty
        curry' = flip . foldr . uncurry $ Abs

tyabs :: Parser (SystemFExpr Text)
tyabs = curry' <$> args <*> expr
  where args = symbol '\\' *> many1 typeId <* symbol '.'
        curry' = flip (foldr TyAbs)

-- Parse type expressions
ty :: Parser (Ty Text)
ty = try forall <|> try arrow

forall :: Parser (Ty Text)
forall = curry' <$> args <*> ty
  where args = symbol' "forall" *> many1 typeId <* symbol '.'
        curry' = flip $ foldr TyForAll

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
