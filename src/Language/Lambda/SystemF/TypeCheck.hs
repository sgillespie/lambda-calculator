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
  typecheckTopLevel ctx expr

typecheckTopLevel
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckTopLevel ctx (Let n expr) = typecheckLet ctx n expr
typecheckTopLevel ctx expr = typecheckExpr ctx expr

typecheckLet
  :: (Pretty name, Ord name)
  => Context name
  -> name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckLet ctx _ = typecheckExpr ctx
  
typecheckExpr
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckExpr ctx (Var v) = typecheckVar ctx v
typecheckExpr ctx (VarAnn v ty) = typecheckVarAnn ctx v ty
typecheckExpr ctx (Abs n t body) = typecheckAbs ctx n t body
typecheckExpr ctx (App e1 e2) = typecheckApp ctx e1 e2
typecheckExpr ctx (TyAbs t body) = typecheckTyAbs ctx t body
typecheckExpr ctx (TyApp e ty) = typecheckTyApp ctx e ty
typecheckExpr _ (Let _ _) = throwError ImpossibleError

typecheckVar :: Ord name => Context name -> name -> Typecheck name (Ty name)
typecheckVar ctx = defaultToUnique . typecheckVar' ctx
  where defaultToUnique = maybe (TyVar <$> tyUnique) pure
    
typecheckVarAnn
  :: (Ord name, Pretty name)
  => Context name
  -> name
  -> Ty  name
  -> Typecheck name (Ty name)
typecheckVarAnn ctx var ty = maybe (pure ty) checkContextType maybeTy
  where checkContextType ty'
          | ty' == ty = pure ty
          | otherwise = throwError $ tyMismatchError ty' ty
        maybeTy = typecheckVar' ctx var

typecheckAbs
  :: (Ord name, Pretty name)
  => Context name
  -> name
  -> Ty name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckAbs ctx name ty body = typecheckAbs' ty' (Map.insert name (BindTerm ty') ctx)
  where typecheckAbs' (TyForAll tyName tyBody) ctx' = do
          inner <- typecheckExpr (Map.insert tyName BindTy ctx') body
          pure $ TyForAll tyName (TyArrow tyBody inner)
        typecheckAbs' t ctx' = TyArrow t <$> typecheckExpr ctx' body

        ty' = liftForAlls ty
      
typecheckApp
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckApp ctx e1 e2 = do
  -- Typecheck expressions
  t1 <- typecheckExpr ctx e1
  t2 <- typecheckExpr ctx e2

  (t1AppInput, t1AppOutput) <- case t1 of
    (TyArrow appInput appOutput) -> pure (appInput, appOutput)
    (TyForAll n1 (TyArrow appInput _))
      -> pure (TyForAll n1 appInput, t2)
    _ -> throwError $ TyMismatchError "Not Arrow"

  -- Verify the output of e1 matches the type of e2
  if t1AppInput `isTyEquivalent` t2
    then return t1AppOutput
    else throwError $ tyMismatchError (TyArrow t2 t1AppOutput) (TyArrow t1 t1AppOutput)

typecheckTyAbs
  :: (Ord name, Pretty name)
  => Context name
  -> name
  -> SystemFExpr name
  -> Typecheck name (Ty name)
typecheckTyAbs ctx ty body = TyForAll ty <$> typecheckExpr ctx' body
  where ctx' = Map.insert ty BindTy ctx

typecheckTyApp
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> Ty name
  -> Typecheck name (Ty name)
typecheckTyApp ctx expr ty = do
  -- Clear in-scope type variables
  let ctx' = Map.filter isTyBind ctx
  
  typecheckExpr ctx' expr >>= \case
    TyForAll tyName tyBody -> pure $ substituteTy ty tyName tyBody
    _ -> do
      err <- tyAppMismatchError ctx expr ty
      throwError err

  where
    isTyBind BindTy = False
    isTyBind _ = True

typecheckVar' :: Ord name => Context name -> name -> Maybe (Ty name)
typecheckVar' ctx var = Map.lookup var ctx >>= \case
  BindTerm ty@(TyForAll tyName tyBody)
    | Map.member tyName ctx -> Just tyBody
    | otherwise -> Just ty
  BindTerm ty -> Just ty
  BindTy -> Nothing

liftForAlls :: Ty name -> Ty name
liftForAlls ty = foldr TyForAll res tyNames
  where (tyNames, res) = liftForAlls' ty

liftForAlls' :: Ty name -> ([name], Ty name)
liftForAlls' (TyVar name) = ([], TyVar name)
liftForAlls' (TyForAll name body) = (name:names, body')
  where (names, body') = liftForAlls' body
liftForAlls' (TyArrow t1 t2) = (n1 ++ n2, TyArrow t1' t2')
  where (n1, t1') = liftForAlls' t1
        (n2, t2') = liftForAlls' t2

isTyEquivalent :: Ord name => Ty name -> Ty name -> Bool
isTyEquivalent t1 t2
  | t1 == t2 = True
  | otherwise = case (t1, t2) of
      (TyForAll n1 t1', TyForAll n2 t2') -> (n1, t1') `areForAllsEquivalent` (n2, t2')
      _ -> False

areForAllsEquivalent :: Ord name => (name, Ty name) -> (name, Ty name) -> Bool
areForAllsEquivalent (n1, t1) (n2, t2) = t1 == substituteTy (TyVar n1) n2 t2

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
substitute ty name (VarAnn n ty') = VarAnn n $ substituteTy ty name ty'
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

tyAppMismatchError
  :: (Ord name, Pretty name)
  => Context name
  -> SystemFExpr name
  -> Ty name
  -> Typecheck name LambdaException
tyAppMismatchError ctx expr appTy = tyAppMismatchError' <$> typecheckExpr ctx expr
  where tyAppMismatchError' actual = TyMismatchError
          $ "Cannot apply type "
          <> prettyPrint appTy
          <> " to non-polymorphic type "
          <> prettyPrint actual
