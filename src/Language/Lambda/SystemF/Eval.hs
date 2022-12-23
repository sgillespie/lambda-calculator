module Language.Lambda.SystemF.Eval
  ( evalExpr,
    betaReduce,
    alphaConvert,
    freeVarsOf
  ) where

import Language.Lambda.Shared.Errors
import Language.Lambda.Shared.UniqueSupply (next)
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.State

import Control.Monad.Except (throwError)
import Prettyprinter
import RIO

evalExpr
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalExpr (Abs n ty expr) = Abs n ty <$> evalExpr expr
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
betaReduce e1 e2 = case e1 of
  App e1' e2' -> App <$> betaReduce e1' e2' <*> pure e2
  Abs n _ e1' -> do
    converted <- alphaConvert (freeVarsOf e2) e1'
    evalExpr $ substitute converted n e2
  Let _ _ -> throwError ImpossibleError
  _ -> pure $ App e1 e2

alphaConvert
  :: (Ord name, Pretty name)
  => [name]
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
alphaConvert freeVars (Abs name ty body) = do
  uniques <- getVarUniques
  nextName <- next freeVars uniques
  alphaConvertAbs name ty body freeVars nextName
alphaConvert _ expr = pure expr

substitute
  :: Eq name
  => SystemFExpr name
  -> name
  -> SystemFExpr name
  -> SystemFExpr name
substitute expr forName inExpr
  = case expr of
      (Var n)
        | n == forName -> inExpr
        | otherwise -> expr
      (VarAnn n _)
        | n == forName -> inExpr
        | otherwise -> expr
      (Abs n ty body)
        | n == forName -> expr
        | otherwise -> Abs n ty $ substitute body forName inExpr
      (App e1 e2) -> App (sub e1) (sub e2)
      _ -> inExpr
  where sub expr' = substitute expr' forName inExpr
  
freeVarsOf
  :: (Ord name, Pretty name)
  => SystemFExpr name
  -> [name]
freeVarsOf (Abs n _ expr) = filter (/=n) . freeVarsOf $ expr
freeVarsOf (App e1 e2) = freeVarsOf e1 ++ freeVarsOf e2
freeVarsOf (Var n) = [n]
freeVarsOf (VarAnn n _) = [n]
freeVarsOf (Let _ expr) = freeVarsOf expr
freeVarsOf (TyAbs _ expr) = freeVarsOf expr
freeVarsOf (TyApp expr _) = freeVarsOf expr

alphaConvertAbs
  :: (Ord name, Pretty name)
  => name
  -> Ty name
  -> SystemFExpr name
  -> [name]
  -> name
  -> Typecheck name (SystemFExpr name)
alphaConvertAbs name ty body freeVars nextName
  | name `elem` freeVars = pure $ Abs nextName ty (substitute body name (Var nextName))
  | otherwise = Abs name ty <$> alphaConvert freeVars body
