module Language.Lambda.SystemF.Eval
  ( evalExpr,
    subGlobals,
    betaReduce,
    alphaConvert,
    etaConvert,
    freeVarsOf
  ) where

import Language.Lambda.Shared.Errors
import Language.Lambda.Shared.UniqueSupply (next)
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.State

import Control.Monad.Except (throwError)
import Prettyprinter
import RIO
import qualified RIO.Map as Map

-- | Evaluates an expression
evalExpr
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalExpr = evalTopLevel

-- | Evaluates a top-level expression
evalTopLevel
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalTopLevel (Let n expr) = Let n <$> (subGlobals expr >>= evalInner)
evalTopLevel expr = subGlobals expr >>= evalInner

-- | Evaluates a non top-level expression. Does NOT support Lets
evalInner
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalInner (Abs n ty expr) = Abs n ty <$> evalInner expr
evalInner (App e1 e2) = evalApp e1 e2
evalInner (TyAbs n expr) = TyAbs n <$> evalInner expr
evalInner (TyApp expr ty) = evalTyApp expr ty
evalInner (Let n expr) = throwError . InvalidLet . prettyPrint $ Let n expr
evalInner expr = pure expr

subGlobals :: Ord name => SystemFExpr name -> Typecheck name (SystemFExpr name)
subGlobals expr = getGlobals >>= subGlobals'
  where subGlobals' globals' = case expr of
          Var x -> pure . maybe expr (view _expr) $ globals' Map.!? x
          VarAnn x _ -> pure . maybe expr (view _expr) $ globals' Map.!? x
          App e1 e2 -> App <$> subGlobals e1 <*> subGlobals e2
          Abs name ty expr'
            | Map.member name globals' -> pure expr
            | otherwise -> Abs name ty <$> subGlobals expr'
          _ -> pure expr

evalApp
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalApp e1 e2 = do
  e1' <- evalInner e1
  e2' <- evalInner e2

  betaReduce e1' e2'

evalTyApp
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> Ty name
  -> Typecheck name (SystemFExpr name)
evalTyApp expr ty = case expr of
    TyAbs name inner -> evalInner $ substituteTyInExpr ty name inner
    _ -> TyApp <$> evalInner expr <*> pure ty

betaReduce
  :: (Ord name, Pretty name)
  => SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
betaReduce e1 e2 = case e1 of
  App e1' e2' -> App <$> betaReduce e1' e2' <*> pure e2
  Abs n _ e1' -> do
    converted <- alphaConvert (freeVarsOf e2) e1'
    evalInner $ substitute converted n e2
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

etaConvert :: Ord name => SystemFExpr name -> SystemFExpr name
etaConvert (Abs name ty body) = case body of
  App e1 (Var name')
    | name == name' -> etaConvert e1
    | otherwise -> Abs name ty (App (etaConvert e1) (Var name'))
  body'@Abs{}
    | body' == eta' -> Abs name ty body'
    | otherwise -> etaConvert $ Abs name ty eta'
    where eta' = etaConvert body'
  _ -> Abs name ty $ etaConvert body
etaConvert (App e1 e2) = App (etaConvert e1) (etaConvert e2)
etaConvert expr = expr

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

substituteTyInExpr
  :: Eq name
  => Ty name
  -> name
  -> SystemFExpr name
  -> SystemFExpr name
substituteTyInExpr ty forName inExpr
  = case inExpr of
      VarAnn name ty' -> VarAnn name (substituteTy ty forName ty')
      App e1 e2 -> App (sub e1) (sub e2)
      Abs name ty' expr -> Abs name (substituteTy ty forName ty') (sub expr)
      TyAbs name expr -> TyAbs name (sub expr)
      TyApp expr ty' -> TyApp (sub expr) (substituteTy ty forName ty')
      _ -> inExpr
  where sub = substituteTyInExpr ty forName

substituteTy
  :: Eq name
  => Ty name
  -> name
  -> Ty name
  -> Ty name
substituteTy ty forName inTy
  = case inTy of
      TyVar n
        | n == forName -> ty
        | otherwise -> inTy
      TyArrow t1 t2 -> TyArrow (sub t1) (sub t2)
      TyForAll n ty'
        | n == forName -> inTy
        | otherwise -> TyForAll n (sub ty')
  where sub = substituteTy ty forName
  
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
