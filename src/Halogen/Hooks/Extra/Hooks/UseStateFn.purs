-- | Idea and implementation by Thomas Honeyman. This was copied
-- | over from the Halogen Hooks repository's Examples folder.
module Halogen.Hooks.Extra.Hooks.UseStateFn
  ( useStateFn
  , UseStateFn
  , useModifyState
  )
  where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Halogen.Hooks (Hook, HookM, StateId, UseState, useState)
import Halogen.Hooks as Hooks

newtype UseStateFn a hooks = UseStateFn (UseState a hooks)

derive instance newtypeUseStateFn :: Newtype (UseStateFn a hooks) _

-- | Rather than writing this...
-- | ```
-- | state /\ stateId <- useState 0
-- | let modifyState = Hooks.modify_ stateId
-- | pure (state /\ modifyState)
-- | ```
-- | ... one can write this code:
-- | ```
-- | state /\ modifyState <- useStateFn Hooks.modify_ 0
-- | ```
-- | See also `useModifyState`, which makes this even less boilerplate-y.
-- |
-- | The function argument should be one of these four functions:
-- | - Hooks.modify_
-- | - Hooks.modify
-- | - Hooks.put
-- | - Hooks.get
-- |
useStateFn
  :: forall m a b
   . (StateId a -> b)
  -> a
  -> Hook m (UseStateFn a) (Tuple a b)
useStateFn fn initial = Hooks.wrap Hooks.do
  map fn <$> useState initial

useModifyState
  :: forall m a
   . a
  -> Hook m (UseStateFn a) (Tuple a ((a -> a) -> HookM m Unit))
useModifyState initial =
  useStateFn Hooks.modify_ initial
