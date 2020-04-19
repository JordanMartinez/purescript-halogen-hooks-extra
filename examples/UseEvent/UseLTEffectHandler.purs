module Examples.UseEvent.UseLTEffectHandler where

import Prelude

import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (HookM, useState, useTickEffect, useRef)
import Halogen.Hooks as Hooks

component :: H.Component HH.HTML (Const Void) Unit Unit Aff
component = Hooks.component \_ -> Hooks.do
  -- valueCB = the callback to run when a new event is pushed
  -- unsubscribeCB = callback to run when unsubscribing
  _ /\ ref <- useRef { valueCB: Nothing, unsubscribeCB: Nothing }

  let
    push :: Int -> HookM _ _ Aff Unit
    push value = do
      mbCallback <- liftEffect $ map (_.valueCB) $ Ref.read ref
      for_ mbCallback \callback -> do
        callback setupUnsubscribeCallback value

    setupUnsubscribeCallback :: (HookM _ _ Aff (HookM _ _ Aff Unit)) -> HookM _ _ Aff Unit
    setupUnsubscribeCallback subscribeAndReturnUnsubscribeCallback = do
      mbUnsubscribe <- liftEffect $ map (_.unsubscribeCB) $ Ref.read ref
      case mbUnsubscribe of
        Nothing -> do
          unsubscribeCode <- subscribeAndReturnUnsubscribeCallback
          liftEffect $ Ref.modify_ (_ { unsubscribeCB = Just unsubscribeCode}) ref
        _ -> do
          -- no need to store unsubscriber because
          -- 1. it's already been stored
          -- 2. no one has subscribed to this yet
          pure unit

    setCallback :: Maybe
             (  ((HookM _ _ Aff (HookM _ _ Aff Unit)) -> HookM _ _ Aff Unit)
             -> Int -- pushed value
             -> HookM _ _ Aff Unit -- code that gets run
             )
             -> HookM _ _ Aff (HookM _ _ Aff Unit)
    setCallback callback = do
      liftEffect $ Ref.modify_ (_ { valueCB = callback }) ref
      pure do
        mbUnsubscribe <- liftEffect $ map (_.unsubscribeCB) $ Ref.read ref
        case mbUnsubscribe of
          Just unsubscribeCode -> do
            unsubscribeCode
            liftEffect $ Ref.modify_ (_ { unsubscribeCB = Nothing }) ref
          _ -> do
            pure unit

  state /\ tState <- useState 0

  -- Here, we set a callback that will handle the events emitted.
  -- We chose to use the `useTickEffect` version, but we could
  -- could have used the `useLifecycleEffect`
  Hooks.captures { state } useTickEffect do

    -- Note: if we don't need to unsubscribe from anything,
    -- we would write the following
    --    void $ setCallback $ Just \_ i -> do
    unsubscribe <- setCallback $ Just \subscribeCallback i -> do
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
          push i
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
