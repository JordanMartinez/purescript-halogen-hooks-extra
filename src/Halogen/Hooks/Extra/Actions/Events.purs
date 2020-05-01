module Halogen.Hooks.Extra.Actions.Events where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (HookM)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

preventDefault'
  :: forall m
   . MonadEffect m
  => Event.Event
  -> HookM m Unit
preventDefault' ev = liftEffect (Event.preventDefault ev)

preventDefault
  :: forall event m
   . MonadEffect m
  => (event -> Event.Event)
  -> event
  -> HookM m Unit
preventDefault toEvent ev = preventDefault' (toEvent ev)

preventMouseEvent
  :: forall m
   . MonadEffect m
  => ME.MouseEvent
  -> HookM m Unit
preventMouseEvent = preventDefault ME.toEvent

preventKeyEvent
  :: forall m
   . MonadEffect m
  => KE.KeyboardEvent
  -> HookM m Unit
preventKeyEvent = preventDefault KE.toEvent
