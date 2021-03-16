-- | Idea and implementation by Thomas Honeyman. This was copied
-- | over from the Halogen Hooks repository's Examples folder.
module Halogen.Hooks.Extra.Hooks.UseStateFn
  ( useStateFn
  , UseStateFn
  , useModifyState_
  , useModifyState
  , usePutState
  )
  where

import Prelude

import Data.Tuple (Tuple)
import Halogen.Hooks (Hook, HookM, StateId, UseState, useState, class HookNewtype)
import Halogen.Hooks as Hooks

type UseStateFn' a = UseState a
foreign import data UseStateFn :: Type -> Hooks.HookType
instance hooknewtypeUseStateFn :: HookNewtype (UseStateFn a) (UseStateFn' a)

-- | `useStateFn` allows you to choose a `MonadState` function to pair with
-- | `Hooks.useState` so you don't have to keep re-typing these functions in
-- | your code if you only need to use one of them per piece of state.
-- |
-- | The available functions to choose from are:
-- | - Hooks.modify_
-- | - Hooks.modify
-- | - Hooks.put
-- | - Hooks.get
-- |
-- | For example, rather than writing:
-- | ```
-- | count /\ countIdx <- Hooks.useState 42
-- | -- ...
-- | Hooks.modify_ countIdx (add 1)
-- | ```
-- |
-- | You can write:
-- | ```
-- | count /\ modifyCount <- useStateFn Hooks.modify_ 42
-- | -- ...
-- | modifyCount (add 1)
-- | ```
-- |
-- | See these helper functions for another layer of convenience:
-- | - useModifyState_
-- | - useModifyState
-- | - usePutState
-- |
useStateFn
  :: forall m a b
   . (StateId a -> b)
  -> a
  -> Hook m (UseStateFn a) (Tuple a b)
useStateFn fn initial = Hooks.wrap hook
  where
  hook :: Hook m (UseStateFn' a) (Tuple a b)
  hook = Hooks.do
    map fn <$> useState initial

-- | Just like `useState`, but provides a convenience function for updating
-- | state, rather than a state index to pass to `Hooks.modify_`.
-- |
-- | Example:
-- | ```
-- | count /\ modifyCount <- useModifyState_ 42
-- | -- ...
-- | modifyCount (add 1)
-- | ```
-- |
-- | Instead of:
-- | ```
-- | count /\ countIdx <- Hooks.useState 42
-- | -- ...
-- | Hooks.modify_ countIdx (add 1)
-- | ```
-- |
-- | Shorthand for:
-- | ```
-- | useStateFn Hooks.modify_
-- | ```
-- |
useModifyState_
  :: forall m a
   . a
  -> Hook m (UseStateFn a) (Tuple a ((a -> a) -> HookM m Unit))
useModifyState_ =
  useStateFn Hooks.modify_

-- | Just like `useState`, but provides a convenience function for updating
-- | state, rather than a state index to pass to `Hooks.modify`.
-- |
-- | Example:
-- | ```
-- | count /\ modifyCount <- useModifyState 42
-- | -- ...
-- | newCount <- modifyCount (add 1)
-- | ```
-- |
-- | Instead of:
-- | ```
-- | count /\ countIdx <- Hooks.useState 42
-- | -- ...
-- | newCount <- Hooks.modify countIdx (add 1)
-- | ```
-- |
-- | Shorthand for:
-- | ```
-- | useStateFn Hooks.modify
-- | ```
-- |
useModifyState
  :: forall m a
   . a
  -> Hook m (UseStateFn a) (Tuple a ((a -> a) -> HookM m a))
useModifyState =
  useStateFn Hooks.modify

-- | Just like `useState`, but provides a convenience function for setting
-- | state, rather than a state index to pass to `Hooks.put`.
-- |
-- | Example:
-- | ```
-- | count /\ putCount <- usePutState 42
-- | -- ...
-- | putCount 0
-- | ```
-- |
-- | Instead of:
-- | ```
-- | count /\ countIdx <- Hooks.useState 42
-- | -- ...
-- | Hooks.put countIdx 0
-- | ```
-- |
-- | Shorthand for:
-- | ```
-- | useStateFn Hooks.put
-- | ```
-- |
usePutState
  :: forall m a
   . a
  -> Hook m (UseStateFn a) (Tuple a (a -> HookM m Unit))
usePutState =
  useStateFn Hooks.put
