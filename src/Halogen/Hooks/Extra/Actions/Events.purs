module Halogen.Hooks.Extra.Actions.Events where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (HookM)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

preventDefault'
  :: forall slots output m
   . MonadEffect m
  => Event.Event
  -> HookM slots output m Unit
preventDefault' ev = liftEffect (Event.preventDefault ev)

preventDefault
  :: forall event slots output m
   . MonadEffect m
  => (event -> Event.Event)
  -> event
  -> HookM slots output m Unit
preventDefault toEvent ev = preventDefault' (toEvent ev)

preventMouseEvent
  :: forall slots output m
   . MonadEffect m
  => ME.MouseEvent
  -> HookM slots output m Unit
preventMouseEvent = preventDefault ME.toEvent

preventKeyEvent
  :: forall slots output m
   . MonadEffect m
  => KE.KeyboardEvent
  -> HookM slots output m Unit
preventKeyEvent = preventDefault KE.toEvent
