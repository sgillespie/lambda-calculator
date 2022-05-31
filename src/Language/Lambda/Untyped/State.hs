module Language.Lambda.Untyped.State
  ( EvalState(..),
    Eval(),
    Globals(),
    runEval,
    execEval,
    unsafeExecEval,
    unsafeRunEval,
    globals,
    uniques,
    mkEvalState,
    getGlobals,
    getUniques,
    setGlobals,
    setUniques
  ) where

import Language.Lambda.Shared.Errors
import Language.Lambda.Untyped.Expression 

import Control.Monad.Except
import RIO
import RIO.State
import qualified RIO.Map as Map

-- | The evaluation state
data EvalState name = EvalState
  { esGlobals :: Globals name,
    esUniques :: [name] -- ^ Unused unique names
  }

-- | A stateful computation
type Eval name
  = StateT (EvalState name)
      (Except LambdaException)

-- | A mapping of global variables to expressions
type Globals name = Map name (LambdaExpr name)

-- | Run an evalualation
runEval :: Eval name result -> EvalState name -> Either LambdaException (result, EvalState name)
runEval computation = runExcept . runStateT computation

-- | Run an evalualation, throwing away the final state
execEval :: Eval name result -> EvalState name -> Either LambdaException result
execEval computation = runExcept . evalStateT computation

-- | Run an evaluation. If the result is an error, throws it
unsafeRunEval :: Eval name result -> EvalState name -> (result, EvalState name)
unsafeRunEval computation state'
  = case runEval computation state' of
      Left err -> error $ show err
      Right res -> res
  
-- | Run an evaluation, throwing away the final state. If the result is an error, throws it
unsafeExecEval:: Eval name result -> EvalState name -> result
unsafeExecEval computation state'
  = case execEval computation state' of
      Left err -> impureThrow err
      Right res -> res

-- | Create an EvalState
mkEvalState :: [name] -> EvalState name
mkEvalState = EvalState Map.empty

globals :: Lens' (EvalState name) (Globals name)
globals f state'
  = (\globals' -> state' { esGlobals = globals' })
  <$> f (esGlobals state')

uniques :: Lens' (EvalState name) [name]
uniques f state'
  = (\uniques' -> state' { esUniques = uniques' })
  <$> f (esUniques state')

-- | Access globals from the state monad
getGlobals :: Eval name (Globals name)
getGlobals = gets (^. globals)

-- | Access unique supply from state monad
getUniques :: Eval name [name]
getUniques = gets (^. uniques)

setGlobals :: Globals name -> Eval name ()
setGlobals globals' = modify (& globals .~ globals')

setUniques :: [name] -> Eval name ()
setUniques uniques' = modify (& uniques .~ uniques')
