module Language.Lambda.Untyped.State
  ( EvalState(..),
    Eval(),
    Globals(),
    globals,
    uniques,
    mkEvalState,
    getGlobals,
    getUniques,
    setGlobals,
    setUniques
  ) where

import Language.Lambda.Untyped.Expression 

import RIO
import RIO.State
import qualified RIO.Map as Map

-- | The evaluation state
data EvalState name = EvalState
  { esGlobals :: Globals name,
    esUniques :: [name] -- ^ Unused unique names
  }

-- | A stateful computation
type Eval name = State (EvalState name)

-- | A mapping of global variables to expressions
type Globals name = Map name (LambdaExpr name)

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
