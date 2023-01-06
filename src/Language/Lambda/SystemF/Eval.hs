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
evalTopLevel (Let n expr) = subGlobals expr >>= evalInner >>= evalLet n
evalTopLevel expr = subGlobals expr >>= evalInner 

-- | Evaluates a non top-level expression. Does NOT support Lets
evalInner
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalInner (Abs n ty expr) = Abs n ty <$> evalInner expr
evalInner (App e1 e2) = evalApp e1 e2
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

evalLet
  :: Ord name
  => name
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalLet name expr = modifyGlobals addBinding >> pure (Let name expr)
  -- TODO[sgillespie]: We don't know the type of expr!
  where addBinding = Map.insert name (TypedExpr expr undefined)

evalApp
  :: (Pretty name, Ord name)
  => SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (SystemFExpr name)
evalApp e1 e2 = do
  e1' <- evalInner e1
  e2' <- evalInner e2

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
