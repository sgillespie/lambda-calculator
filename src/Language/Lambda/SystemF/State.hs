module Language.Lambda.SystemF.State
  ( TypecheckState(..),
    Typecheck(),
    Context(),
    Binding(..),
    Globals(),
    runTypecheck,
    execTypecheck,
    unsafeRunTypecheck,
    unsafeExecTypecheck,
    mkTypecheckState,
    _context,
    _globals,
    _varUniques,
    _tyUniques,
    getContext,
    getGlobals,
    getVarUniques,
    getTyUniques,
    modifyGlobals,
    modifyVarUniques,
    modifyTyUniques,
    setGlobals,
    setVarUniques,
    setTyUniques
  ) where

import Language.Lambda.Shared.Errors (LambdaException(..))
import Language.Lambda.SystemF.Expression

import Control.Monad.Except (Except(), runExcept)
import RIO
import RIO.State
import qualified RIO.Map as Map

data TypecheckState name = TypecheckState
  { tsGlobals :: Globals name,
    tsVarUniques :: [name],  -- ^ A unique supply of term-level variables
    tsTyUniques :: [name]    -- ^ A unique supply of type-level variables
  } deriving (Eq, Show)

type Typecheck name
  = StateT (TypecheckState name)
      (Except LambdaException)

type Globals name = Map name (TypedExpr name)

type Context name = Map name (Binding name)

data Binding name
  = BindTerm (Ty name)
  | BindTy
  deriving (Eq, Show)

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

_context :: SimpleGetter (TypecheckState name) (Context name)
_context = to (getContext' . tsGlobals)
  where getContext' :: Globals name -> Context name
        getContext' = Map.map (\expr -> BindTerm (expr ^. _ty))
        
_globals :: Lens' (TypecheckState name) (Globals name)
_globals f state' = (\globals' -> state' { tsGlobals = globals' })
  <$> f (tsGlobals state')

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

getGlobals :: Typecheck name (Globals name)
getGlobals = gets (^. _globals)

modifyGlobals :: (Globals name -> Globals name) -> Typecheck name ()
modifyGlobals f = modify $ _globals %~ f

modifyVarUniques :: ([name] -> [name]) -> Typecheck name ()
modifyVarUniques f = modify $ _varUniques %~ f

modifyTyUniques :: ([name] -> [name]) -> Typecheck name ()
modifyTyUniques f = modify $ _tyUniques %~ f

setVarUniques :: [name] -> Typecheck name ()
setVarUniques uniques' = modify $ _varUniques .~ uniques'

setTyUniques :: [name] -> Typecheck name ()
setTyUniques uniques' = modify $ _tyUniques .~ uniques'

setGlobals :: Globals name -> Typecheck name ()
setGlobals globals' = modify $ _globals .~ globals'
