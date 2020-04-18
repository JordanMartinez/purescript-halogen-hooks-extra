module Examples.UseEvent.UseLTEffectHandler where

import Prelude

import Data.Const (Const)
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (HookM, useState, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks.UseEvent (useEvent)

component :: H.Component HH.HTML (Const Void) Unit Unit Aff
component = Hooks.component \_ -> Hooks.do
  changes <- useEvent

  state /\ tState <- useState 0

  let
    pushValueAndTriggerCallback :: Int -> HookM _ _ Aff Unit
    pushValueAndTriggerCallback = changes.push

  -- at some point in code, we trigger the callback via
  -- `pushValueAndTriggerCallback`

  -- Here, we set a callback that will handle the events emitted.
  -- We chose to use the `useTickEffect` version, but we could
  -- could have used the `useLifecycleEffect`
  Hooks.captures { state } useTickEffect do

    -- Note: if we don't need to unsubscribe from anything,
    -- we can ignore the `unsubscribeCallback`. For example
    --  changes.setCallback $ Just \_ i -> do
    changes.setCallback $ Just \unsubscribeCallback i -> do
      -- here, we handle the event emitted
      liftEffect $ log $ "Handling event. New value is: " <> show i

      -- here, we can set up some resources and do various things

      -- here, we store the code necessary for unsubscribing
      unsubscribeCallback do
        let unsubscribe' = pure unit -- unsubscribe
        unsubscribe'

    pure $ Just do
      changes.unsubscribe

  Hooks.pure $
    HH.div_
      [ HH.button
        [ HE.onClick \_ -> Just do
          i <- liftEffect $ randomInt 0 10
          pushValueAndTriggerCallback i
        ]
        [ HH.text "Click to push a new int value to callback"
        ]
      , HH.br_
      , HH.button
        [ HE.onClick \_ -> Just do
          Hooks.modify_ tState \s -> s + 1
        ]
        [ HH.text "Click to change the state value, which will unsubscribe \
                  \prior handler and resubscribe using new handler"
        ]
      ]
