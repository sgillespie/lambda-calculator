module Language.Lambda.SystemF.State
  ( TypecheckState(..),
    Typecheck(),
    Context(),
    runTypecheck,
    execTypecheck,
    unsafeRunTypecheck,
    unsafeExecTypecheck,
    mkTypecheckState,
    context,
    uniques,
    getContext,
    getUniques,
    modifyContext,
    modifyUniques,
    setContext,
    setUniques
  ) where

import Language.Lambda.Shared.Errors (LambdaException(..))
import Language.Lambda.SystemF.Expression (Ty(..))

import Control.Monad.Except (Except(), runExcept)
import RIO
import RIO.State
import qualified RIO.Map as Map

data TypecheckState name = TypecheckState
  { tsContext :: Context name,
    tsUniques :: [name]
  }

type Typecheck name
  = StateT (TypecheckState name)
      (Except LambdaException)

type Context name = Map name (Ty name)

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

mkTypecheckState :: [name] -> TypecheckState name
mkTypecheckState = TypecheckState Map.empty

uniques :: Lens' (TypecheckState name) [name]
uniques f state' = (\uniques' -> state' { tsUniques = uniques' })
  <$> f (tsUniques state')

context :: Lens' (TypecheckState name) (Context name)
context f state' = (\context' -> state' { tsContext = context' })
  <$> f (tsContext state')

getUniques :: Typecheck name [name]
getUniques = gets (^. uniques)

getContext :: Typecheck name (Context name)
getContext = gets (^. context)

modifyContext :: (Context name -> Context name) -> Typecheck name ()
modifyContext f = modify $ context %~ f

modifyUniques :: ([name] -> [name]) -> Typecheck name ()
modifyUniques f = modify $ uniques %~ f

setUniques :: [name] -> Typecheck name ()
setUniques uniques' = modify $ uniques .~ uniques'

setContext :: Context name -> Typecheck name ()
setContext context' = modify $ context .~ context'
