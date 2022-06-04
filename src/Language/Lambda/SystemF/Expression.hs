module Language.Lambda.SystemF.Expression
  ( SystemFExpr(..),
    Ty(..),
    prettyPrint,
    upperLambda
  ) where

import Data.Monoid
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import RIO

data SystemFExpr name ty
  -- | Variable: `x`
  = Var name
  -- | Function application: `x y`
  | App (SystemFExpr name ty) (SystemFExpr name ty)
  -- | Lambda abstraction: `\x: X. x`
  | Abs name (Ty ty) (SystemFExpr name ty)
  -- | Type Abstraction: `\X. body`
  | TyAbs ty (SystemFExpr name ty)                  
  -- | Type Application: `x [X]`
  | TyApp (SystemFExpr name ty) (Ty ty)
  deriving (Eq, Show)

data Ty name
  = TyVar name                  -- ^ Type variable (T)
  | TyArrow (Ty name) (Ty name) -- ^ Type arrow    (T -> U)
  | TyForAll name (Ty name)     -- ^ Universal type (forall T. X)
  deriving (Eq, Show)

instance (Pretty name, Pretty ty) => Pretty (SystemFExpr name ty) where
  pretty (Var name) = pretty name
  pretty (App e1 e2) = prettyApp e1 e2
  pretty (Abs name ty body) = prettyAbs name ty body
  pretty (TyAbs ty body) = prettyTyAbs ty body
  pretty (TyApp expr ty) = prettyTyApp expr ty

instance Pretty name => Pretty (Ty name) where
  pretty = prettyTy False

prettyPrint :: Pretty pretty => pretty -> Text
prettyPrint expr = renderStrict docStream
  where docStream = layoutPretty defaultLayoutOptions (pretty expr)

upperLambda :: Char
upperLambda = 'Λ'

prettyApp
  :: (Pretty name, Pretty ty)
  => SystemFExpr name ty
  -> SystemFExpr name ty
  -> Doc a
prettyApp e1@Abs{} e2@Abs{} = parens (pretty e1) <+> parens (pretty e2)
prettyApp e1@Abs{} e2 = parens (pretty e1) <+> pretty e2
prettyApp e1 e2@Abs{} = pretty e1 <+> parens (pretty e2)
prettyApp e1 e2@App{} = pretty e1 <+> parens (pretty e2)
prettyApp e1 e2 = pretty e1 <+> pretty e2

prettyAbs
  :: (Pretty name, Pretty ty)
  => name
  -> Ty ty
  -> SystemFExpr name ty
  -> Doc ann
prettyAbs name ty body
  = lambda
    <+> hsep (map (uncurry prettyArg) names)
    <> dot
    <+> pretty body'
  where (names, body') = uncurryAbs name ty body

prettyTyAbs :: (Pretty name, Pretty ty) => ty -> SystemFExpr name ty -> Doc ann
prettyTyAbs name body = upperLambda' <+> hsep (map pretty names) <> dot
    <+> pretty body'
  where (names, body') = uncurryTyAbs name body
prettyTyApp :: (Pretty name, Pretty ty) => SystemFExpr name ty -> Ty ty -> Doc ann
prettyTyApp expr ty = pretty expr <+> brackets (pretty ty)

prettyTy :: Pretty name => Bool -> Ty name -> Doc ann
prettyTy _ (TyVar name) = pretty name
prettyTy compact (TyArrow t1 t2) = prettyTyArrow compact t1 t2
prettyTy compact (TyForAll name ty) = prettyTyForAll compact name ty

prettyTyArrow :: Pretty name => Bool -> Ty name -> Ty name -> Doc ann
prettyTyArrow compact (TyArrow t1 t2) t3
  = prettyTyArrow' compact compositeTy $ prettyTy compact t3
  where compositeTy = parens $ prettyTyArrow compact t1 t2

prettyTyArrow compact t1 t2
  = prettyTyArrow' compact (prettyTy compact t1) (prettyTy compact t2)

prettyTyForAll :: Pretty name => Bool -> name -> Ty name -> Doc ann
prettyTyForAll compact name ty
  = "forall"
  <+> pretty name <> dot
  <+> prettyTy compact ty

lambda :: Doc ann
lambda = pretty 'λ'

prettyArg :: (Pretty name, Pretty ty) => name -> Ty ty -> Doc ann
prettyArg name (TyArrow t1 t2)
  = pretty name <> colon <> parens (prettyTyArrow True t1 t2)
prettyArg name ty = pretty name <> colon <> pretty ty

upperLambda' :: Doc ann
upperLambda' = pretty upperLambda

prettyTyArrow' :: Bool -> Doc ann -> Doc ann -> Doc ann
prettyTyArrow' compact doc1 doc2 = doc1 `add'` "->" `add'` doc2
  where add'
          | compact = (<>) 
          | otherwise = (<+>)

uncurryAbs :: n -> Ty t -> SystemFExpr n t -> ([(n, Ty t)], SystemFExpr n t)
uncurryAbs name ty = uncurry' [(name, ty)] 
  where uncurry' ns (Abs n' t' body') = uncurry' ((n', t'):ns) body'
        uncurry' ns body'             = (reverse ns, body')

uncurryTyAbs :: t -> SystemFExpr n t -> ([t], SystemFExpr n t)
uncurryTyAbs ty = uncurry' [ty]
  where uncurry' ts (TyAbs t' body') = uncurry' (t':ts) body'
        uncurry' ts body'            = (reverse ts, body')
