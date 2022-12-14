module Language.Lambda.SystemF.TypeCheck where

import Language.Lambda.Shared.Errors (LambdaException(..))
import Language.Lambda.SystemF.Expression
import Language.Lambda.SystemF.State

import Control.Monad.Except (MonadError(..))
import Prettyprinter
import RIO
import qualified RIO.Map as Map

type UniqueSupply n = [n]
type Context' n t = Map n t

typecheck
  :: (Ord name, Pretty name)
  => SystemFExpr name
  -> Typecheck name (Ty name)
typecheck expr = do
  ctx <- getContext
  typecheck' ctx expr

typecheck'
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheck' ctx (Var v) = typecheckVar ctx v
typecheck' ctx (VarAnn v ty) = typecheckVarAnn ctx v ty
typecheck' ctx (Abs n t body) = typecheckAbs ctx n t body
typecheck' ctx (App e1 e2) = typecheckApp ctx e1 e2
typecheck' ctx (TyAbs t body) = typecheckTyAbs ctx t body
typecheck' ctx (TyApp e ty) = typecheckTyApp ctx e ty

typecheckVar :: Ord name => Context name -> name -> Typecheck name (Ty name)
typecheckVar ctx var = defaultToFreshTyVar (Map.lookup var ctx)
  where defaultToFreshTyVar (Just v) = pure v
        defaultToFreshTyVar Nothing = TyVar <$> tyUnique

typecheckVarAnn
  :: (Ord name, Pretty name)
  => Context name
  -> name
  -> Ty  name
  -> Typecheck name (Ty name)
typecheckVarAnn ctx var ty = maybe (pure ty) checkContextType (Map.lookup var ctx)
  where checkContextType ty'
          | ty' == ty = pure ty
          | otherwise = throwError $ tyMismatchError ty' ty

typecheckAbs
  :: (Ord name, Pretty name)
  => Context name
  -> name
  -> Ty name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckAbs ctx name ty body = TyArrow ty <$> typecheck' ctx' body
  where ctx' = Map.insert name ty ctx

typecheckApp
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckApp ctx e1 e2 = do
  -- Typecheck expressions
  t1 <- typecheck' ctx e1
  t2 <- typecheck' ctx e2

  -- Verify the type of t1 is an Arrow
  (t1AppInput, t1AppOutput) <- case t1 of
    (TyArrow appInput appOutput) -> return (appInput, appOutput)
    _ -> throwError $ tyMismatchError (TyArrow t2 t1) t1

  -- Verify the output of e1 matches the type of e2
  if t1AppInput == t2
    then return t1AppOutput
    else throwError $ tyMismatchError (TyArrow t2 t1AppOutput) (TyArrow t1 t1AppOutput)

typecheckTyAbs
  :: (Ord name, Pretty name)
  => Context name
  -> name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckTyAbs ctx ty body = TyForAll ty <$> typecheck' ctx' body
  where ctx' = Map.insert ty (TyVar ty) ctx

typecheckTyApp
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> Ty name
  -> Typecheck name (Ty name)
typecheckTyApp ctx (TyAbs t expr) ty = typecheck' ctx $ substitute ty t expr
typecheckTyApp ctx expr _ = typecheck' ctx expr

tyUnique :: Typecheck name name
tyUnique = getTyUniques >>= tyUnique'
    where tyUnique' (u:us) = setTyUniques us $> u
          tyUnique' _ = throwError ImpossibleError

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
