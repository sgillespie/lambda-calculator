module Language.Lambda.SystemF.TypeCheck where

import Language.Lambda.Shared.Errors (LambdaException(..))
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.State

import Control.Monad.Except (MonadError(..))
import Prettyprinter
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

type UniqueSupply n = [n]
type Context' n t = Map n t

-- TODO: name/ty different types
typecheck
  :: (Ord name, Pretty name)
  => SystemFExpr name
  -> Typecheck name (Ty name)
typecheck (Var v) = typecheckVar v
typecheck (Abs n t body) = typecheckAbs n t body
typecheck (App e1 e2) = typecheckApp e1 e2
typecheck (TyAbs t body) = typecheckTyAbs t body
typecheck (TyApp e ty) = typecheckTyApp e ty

typecheckVar :: Ord name => name -> Typecheck name (Ty name)
typecheckVar var = getContext >>= defaultToFreshTyVar . Map.lookup var
  where defaultToFreshTyVar (Just v) = return v
        defaultToFreshTyVar Nothing = TyVar <$> unique

typecheckAbs
  :: (Ord name, Pretty name)
  => name
  -> Ty name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckAbs name ty body
  = modifyContext (Map.insert name ty)
    >> TyArrow ty <$> typecheck body

typecheckApp
  :: (Ord name, Pretty name)
  => SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckApp e1 e2 = do
  -- Typecheck expressions
  t1 <- typecheck e1
  t2 <- typecheck e2

  -- Verify the type of t1 is an Arrow
  (t1AppInput, t1AppOutput) <- case t1 of
    (TyArrow appInput appOutput) -> return (appInput, appOutput)
    t1' -> throwError $ tyMismatchError t1' t1

  -- Verify the output of e1 matches the type of e2
  if t1AppInput == t2
    then return t1AppOutput
    else throwError $ tyMismatchError (TyArrow t2 t1AppOutput) (TyArrow t1 t1AppOutput)

typecheckTyAbs
  :: (Ord name, Pretty name)
  => name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckTyAbs ty body
  = modifyContext (Map.insert ty (TyVar ty))
    >> TyForAll ty <$> typecheck body

typecheckTyApp
  :: (Ord name, Pretty name)
  => SystemFExpr name
  -> Ty name
  -> Typecheck name (Ty name)
typecheckTyApp (TyAbs t expr) ty = typecheck $ substitute ty t expr
typecheckTyApp expr _ = typecheck expr

unique :: Typecheck name name
unique = getUniques >>= fromJust' . List.headMaybe
  where fromJust' (Just u) = return u
        fromJust' Nothing = throwError ImpossibleError

substitute
  :: Eq n
  => Ty n
  -> n
  -> SystemFExpr n
  -> SystemFExpr n
substitute ty name (App e1 e2) = App (substitute ty name e1) (substitute ty name e2)
substitute ty name (Abs n ty' e) = Abs n (substituteTy ty name ty') (substitute ty name e)
substitute ty name (TyAbs ty' e) = TyAbs ty' (substitute ty name e) 
substitute ty name (TyApp e ty') = TyApp (substitute ty name e) (substituteTy ty name ty')
substitute _ _ expr = expr

substituteTy
  :: Eq name
  => Ty name
  -> name
  -> Ty name
  -> Ty name
substituteTy ty name (TyArrow t1 t2) 
  = TyArrow (substituteTy ty name t1) (substituteTy ty name t2)
substituteTy ty name ty'@(TyVar name') 
  | name == name' = ty
  | otherwise     = ty'
substituteTy _ name t2@(TyForAll name' t2') 
  | name == name' = t2
  | otherwise     = TyForAll name' (substituteTy t2 name t2')

tyMismatchError
  :: Pretty ty => ty -> ty -> LambdaException
tyMismatchError expected actual
  = TyMismatchError
  $ "Couldn't match expected type "
  <> prettyPrint expected
  <> " with actual type "
  <> prettyPrint actual
