module Language.Lambda.Untyped.Expression
  ( LambdaExpr(..),
    prettyPrint
  ) where

import RIO
import Prettyprinter
import Prettyprinter.Render.Text (renderLazy)
import qualified RIO.Text.Lazy as Text

data LambdaExpr name
  = Var name                                -- ^ Variables
  | App (LambdaExpr name) (LambdaExpr name) -- ^ Application
  | Abs name (LambdaExpr name)              -- ^ Abstractions
  | Let name (LambdaExpr name)              -- ^ Let bindings
  deriving (Eq, Show)

instance Pretty name => Pretty (LambdaExpr name) where
  pretty (Var name) = pretty name
  pretty (Abs name body) = prettyAbs name body
  pretty (App e1 e2) = prettyApp e1 e2
  pretty (Let name body) = prettyLet name body

prettyPrint :: Pretty name => LambdaExpr name -> Text.Text
prettyPrint expr = renderLazy docStream
  where docStream = layoutPretty defaultLayoutOptions (pretty expr)

prettyAbs :: Pretty name => name -> LambdaExpr name -> Doc a
prettyAbs name body
  = lambda' <> hsep (map pretty names) <> dot
    <+> pretty body'
  where (names, body') = uncurryAbs name body

prettyApp :: Pretty name => LambdaExpr name -> LambdaExpr name -> Doc a
prettyApp e1@(Abs _ _) e2@(Abs _ _) = parens (pretty e1) <+> parens (pretty e2)
prettyApp e1@(Abs _ _) e2 = parens (pretty e1) <+> pretty e2
prettyApp e1 e2@(Abs _ _) = pretty e1 <+> parens (pretty e2)
prettyApp e1 e2@(App _ _) = pretty e1 <+> parens (pretty e2)
prettyApp e1 e2 = pretty e1 <+> pretty e2

prettyLet :: Pretty name => name -> LambdaExpr name -> Doc a
prettyLet name body
  = pretty ("let"::Text)
    <+> pretty name
    <+> "="
    <+> pretty body

lambda' :: Doc ann
lambda' = pretty 'λ'

uncurryAbs :: n -> LambdaExpr n -> ([n], LambdaExpr n)
uncurryAbs n = uncurry' [n]
  where uncurry' ns (Abs n' body') = uncurry' (n':ns) body'
        uncurry' ns body'          = (reverse ns, body')
