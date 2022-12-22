module Language.Lambda.SystemF.Eval
  (evalExpr) where

import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.State

import Prettyprinter
import RIO

evalExpr
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalExpr (App e1 e2) = evalApp e1 e2
evalExpr expr = pure expr

evalApp
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalApp e1 e2 = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2

  betaReduce e1' e2'

betaReduce
  :: (Ord name, Pretty name)
  => SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
betaReduce (Abs n _ e1) e2 = do
  subbed <- substitute e1 n e2
  evalExpr subbed
betaReduce e1 e2 = pure $ App e1 e2

substitute
  :: Eq name
  => SystemFExpr name
  -> name
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
substitute expr forName inExpr
  = case expr of
      (Var n)
        | n == forName -> pure inExpr
        | otherwise -> pure expr
      (Abs n ty body)
        | n == forName -> pure expr
        | otherwise -> Abs n ty <$> substitute body forName inExpr
      (App e1 e2) -> App <$> sub e1 <*> sub e2
      _ -> pure inExpr
  where sub expr' = substitute expr' forName inExpr
  
    
