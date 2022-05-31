module Language.Lambda.Untyped.Eval
  ( EvalState(..),
    evalExpr,
    subGlobals,
    betaReduce,
    alphaConvert,
    etaConvert,
    freeVarsOf
  ) where

import Control.Monad.Except
import Prettyprinter
import RIO
import RIO.List (find)
import qualified RIO.Map as Map

import Language.Lambda.Shared.Errors
import Language.Lambda.Untyped.Expression
import Language.Lambda.Untyped.State

-- | Evaluate an expression
evalExpr :: (Pretty name, Ord name) => LambdaExpr name -> Eval name (LambdaExpr name)
evalExpr (Let name expr) = do
  globals' <- getGlobals
  result <- evalExpr' $ subGlobals globals' expr

  setGlobals $ Map.insert name result globals'

  return $ Let name result

evalExpr expr = do
  globals' <- getGlobals
  evalExpr' $ subGlobals globals' expr

-- | Evaluate an expression; does not support `let`
evalExpr' :: (Eq name, Pretty name) => LambdaExpr name -> Eval name (LambdaExpr name)
evalExpr' expr@(Var _) = return expr
evalExpr' (Abs name expr) = Abs name <$> evalExpr' expr
evalExpr' (App e1 e2) = do
  e1' <- evalExpr' e1
  e2' <- evalExpr' e2
  betaReduce e1' e2'
evalExpr' expr@(Let _ _) = throwError . InvalidLet . prettyPrint $ expr

-- | Look up free vars that have global bindings and substitute them
subGlobals
  :: Ord name
  => Map name (LambdaExpr name)
  -> LambdaExpr name
  -> LambdaExpr name
subGlobals globals' expr@(Var x) = Map.findWithDefault expr x globals'
subGlobals globals' (App e1 e2) = App (subGlobals globals' e1) (subGlobals globals' e2)
subGlobals globals' (Abs name expr) = Abs name expr'
  where expr'
          | Map.member name globals' = expr
          | otherwise = subGlobals globals' expr
subGlobals _ expr = expr

-- | Function application
betaReduce
  :: (Eq name, Pretty name)
  => LambdaExpr name
  -> LambdaExpr name
  -> Eval name (LambdaExpr name)
betaReduce expr@(Var _) e2 = return $ App expr e2
betaReduce (App e1 e1') e2 = do
  reduced <- betaReduce e1 e1'
  return $ App reduced e2
betaReduce (Abs n e1) e2 = do
  e1' <- alphaConvert (freeVarsOf e2) e1
  evalExpr' $ substitute e1' n e2
betaReduce _ _ = throwError ImpossibleError

-- | Rename abstraction parameters to avoid name captures
alphaConvert :: Eq name => [name] -> LambdaExpr name -> Eval name (LambdaExpr name)
alphaConvert freeVars (Abs name body) = do
  uniques' <- getUniques
  let nextVar = fromMaybe name $ find (`notElem` freeVars) uniques'

  if name `elem` freeVars
    then return $ Abs nextVar (substitute body name (Var nextVar))
    else Abs name <$> alphaConvert freeVars body

alphaConvert _ expr = return expr

-- | Eliminite superfluous abstractions
etaConvert :: Eq n => LambdaExpr n -> LambdaExpr n
etaConvert (Abs n (App e1 (Var n')))
  | n == n'   = etaConvert e1
  | otherwise = Abs n (App (etaConvert e1) (Var n'))
etaConvert (Abs n e@(Abs _ _)) 
  -- If `etaConvert e == e` then etaConverting it will create an infinite loop
  | e == e'   = Abs n e'
  | otherwise = etaConvert (Abs n e')
  where e' = etaConvert e
etaConvert (Abs n expr) = Abs n (etaConvert expr)
etaConvert (App e1 e2)  = App (etaConvert e1) (etaConvert e2)
etaConvert expr = expr

-- | Substitute an expression for a variable name in another expression
substitute :: Eq name => LambdaExpr name -> name -> LambdaExpr name -> LambdaExpr name
substitute subExpr@(Var name) subName inExpr
  | name == subName = inExpr
  | otherwise = subExpr

substitute subExpr@(Abs name expr) subName inExpr
  | name == subName = subExpr
  | otherwise = Abs name (substitute expr subName inExpr)

substitute (App e1 e2) subName inExpr
  = App (sub e1) (sub e2)
  where sub expr = substitute expr subName inExpr

substitute _ _ expr = expr

-- | Find the free variables in an expression
freeVarsOf :: Eq n => LambdaExpr n -> [n]
freeVarsOf (Abs n expr) = filter (/=n) . freeVarsOf $ expr
freeVarsOf (App e1 e2)  = freeVarsOf e1 ++ freeVarsOf e2
freeVarsOf (Var n)      = [n]
freeVarsOf _ = []
