module Examples.UseEvent.UseLTEffectHandler where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (useState, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks.UseEvent (useEvent)

component :: H.Component HH.HTML (Const Void) Unit Unit Aff
component = Hooks.component \_ -> Hooks.do
  changes <- useEvent
  state /\ tState <- useState 0

  -- Here, we set a callback that will handle the events emitted.
  -- We chose to use the `useTickEffect` version, but we could
  -- could have used the `useLifecycleEffect`
  Hooks.captures { state } useTickEffect do

    -- Note: if we don't need to unsubscribe from anything,
    -- we would write the following
    --    void $ setCallback $ Just \_ i -> do
    unsubscribe <- changes.setCallback $ Just \subscribeCallback i -> do
      -- here, we handle the event emitted
      liftEffect $ log $ "Handling event. New value is: " <> show i

      subscribeCallback do
        -- here, we can set up some resources and do various things
        liftEffect $ log $
          "Setting up resources. You should see this \
          \message appear on the first run and every time the state value \
          \changes. However, it should occur only AFTER the unsubscribe \
          \message appears."
        pure do
          -- here, we store the code necessary for unsubscribing
          liftEffect $ log $
            "Cleaning up resources. You should see this \
            \message appear on the last run and every time the state values \
            \changes. However, it should occur only BEFORE the subscribe \
            \message appears."

    pure $ Just do
      -- Here, we take the unsubscribe code we specified above
      -- and run it before the next tick occurs.
      unsubscribe

  Hooks.pure $
    HH.div_
      [ HH.button
        [ HE.onClick \_ -> Just do
          i <- liftEffect $ randomInt 0 10
          changes.push i
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
