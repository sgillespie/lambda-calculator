module Language.Lambda.SystemF.State
  ( TypecheckState(..),
    Typecheck(),
    Context(),
    Globals(),
    runTypecheck,
    execTypecheck,
    unsafeRunTypecheck,
    unsafeExecTypecheck,
    mkTypecheckState,
    _context,
    _varUniques,
    _tyUniques,
    getContext,
    getVarUniques,
    getTyUniques,
    modifyContext,
    modifyVarUniques,
    modifyTyUniques,
    setContext,
    setVarUniques,
    setTyUniques
  ) where

import Language.Lambda.Shared.Errors (LambdaException(..))
import Language.Lambda.SystemF.Expression (Ty(..), TypedExpr(..))

import Control.Monad.Except (Except(), runExcept)
import RIO
import RIO.State
import qualified RIO.Map as Map

data TypecheckState name = TypecheckState
  { tsContext :: Context name,
    tsVarUniques :: [name],  -- ^ A unique supply of term-level variables
    tsTyUniques :: [name] -- ^ A unique supply of type-level variables
  }

type Typecheck name
  = StateT (TypecheckState name)
      (Except LambdaException)

type Context name = Map name (Ty name)

type Globals name = Map name (TypedExpr name)

runTypecheck
  :: Typecheck name result
  -> TypecheckState name
  -> Either LambdaException (result, TypecheckState name)
runTypecheck computation = runExcept . runStateT computation

execTypecheck
  :: Typecheck name result
  -> TypecheckState name
  -> Either LambdaException result
execTypecheck computation = runExcept . evalStateT computation

unsafeRunTypecheck
  :: Typecheck name result
  -> TypecheckState name
  -> (result, TypecheckState name)
unsafeRunTypecheck computation state' = either impureThrow id tcResult
  where tcResult = runTypecheck computation state'

unsafeExecTypecheck :: Typecheck name result -> TypecheckState name -> result
unsafeExecTypecheck computation state' = either impureThrow id tcResult
  where tcResult = execTypecheck computation state'

mkTypecheckState :: [name] -> [name] -> TypecheckState name
mkTypecheckState = TypecheckState Map.empty

_context :: Lens' (TypecheckState name) (Context name)
_context f state' = (\context' -> state' { tsContext = context' })
  <$> f (tsContext state')

_varUniques :: Lens' (TypecheckState name) [name]
_varUniques f state' = (\uniques' -> state' { tsVarUniques = uniques' })
  <$> f (tsVarUniques state')

_tyUniques :: Lens' (TypecheckState name) [name]
_tyUniques f state' = (\uniques' -> state' { tsTyUniques = uniques' })
  <$> f (tsTyUniques state')

getVarUniques :: Typecheck name [name]
getVarUniques = gets (^. _varUniques)

getTyUniques :: Typecheck name [name]
getTyUniques = gets (^. _tyUniques)

getContext :: Typecheck name (Context name)
getContext = gets (^. _context)

modifyContext :: (Context name -> Context name) -> Typecheck name ()
modifyContext f = modify $ _context %~ f

modifyVarUniques :: ([name] -> [name]) -> Typecheck name ()
modifyVarUniques f = modify $ _varUniques %~ f

modifyTyUniques :: ([name] -> [name]) -> Typecheck name ()
modifyTyUniques f = modify $ _tyUniques %~ f

setVarUniques :: [name] -> Typecheck name ()
setVarUniques uniques' = modify $ _varUniques .~ uniques'

setTyUniques :: [name] -> Typecheck name ()
setTyUniques uniques' = modify $ _tyUniques .~ uniques'

setContext :: Context name -> Typecheck name ()
setContext context' = modify $ _context .~ context'
