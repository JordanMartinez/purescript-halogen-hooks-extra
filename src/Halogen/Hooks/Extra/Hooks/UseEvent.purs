module Halogen.Hooks.Extra.Hooks.UseEvent
  ( useEvent
  , subscribeTo
  , UseEvent
  , EventApi
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Hooks (Hook, HookM, UseRef, useRef)
import Halogen.Hooks as Hooks

newtype UseEvent slots output m a hooks = UseEvent (UseRef (Maybe (a -> HookM slots output m Unit)) hooks)

derive instance newtypeUseEvent :: Newtype (UseEvent slots output m a hooks) _

-- | Authors of hooks should use `push` to push events into the handler.
-- | They don't return `push` in their custom hook, but instead return
-- | `props`. The end-user will use `props` as an argument to `subscribeTo`.
type EventApi slots output m a =
  { push :: a -> HookM slots output m Unit
  , callbackRef :: Ref (Maybe (a -> HookM slots output m Unit))
  }

-- | Allows you to "push" events that occur inside a hook
-- | to a single handler outside of the hook. This allows the end user
-- | to use the full API returned by the hook when handling the event.
-- | ```
-- | -- in the custom Hook code...
-- | onSomeEvent <- useEvent
-- |
-- | -- somewhere in your HookM code
-- |   onSomeEvent.push "user clicked foo"
-- |
-- | Hooks.pure
-- |   { onSomeEvent: onSomeEvent.callbackRef }
-- |
-- | --------------
-- | -- in end user Hook code
-- |
-- | someLib <- useSomeLibHook
-- | useLifecycleEffect do
-- |   subscribeTo someLib.onSomeEvent \string -> do
-- |     Hooks.raise ("Event occurred: " <> string)
-- | ```
useEvent
  :: forall output m slots a
   . MonadEffect m
  => Hook slots output m (UseEvent slots output m a) (EventApi slots output m a)
useEvent = Hooks.wrap Hooks.do
  _ /\ tRef <- useRef Nothing

  Hooks.pure
    { push: \value -> do
        mbCallback <- liftEffect $ Ref.read tRef
        for_ mbCallback \callback -> callback value
    , callbackRef: tRef
    }

subscribeTo
  :: forall slots output m a
   . MonadEffect m
  => Ref (Maybe (a -> HookM slots output m Unit))
  -> (a -> HookM slots output m Unit)
  -> HookM slots output m Unit
subscribeTo callbackRef handler =
  liftEffect $ Ref.write (Just handler) callbackRef
