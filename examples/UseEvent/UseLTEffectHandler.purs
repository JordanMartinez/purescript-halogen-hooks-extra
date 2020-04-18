module Examples.UseEvent.UseLTEffectHandler where

import Prelude

import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (HookM, useRef, useState, useTickEffect)
import Halogen.Hooks as Hooks

component :: H.Component HH.HTML (Const Void) Unit Unit Aff
component = Hooks.component \_ -> Hooks.do
  _ /\ unsubscribeRef <- useRef Nothing
  _ /\ callbackRef <- useRef Nothing

  state /\ tState <- useState 0

  let
    pushValueAndTriggerCallback :: Int -> HookM _ _ Aff Unit
    pushValueAndTriggerCallback value = do
      mbCallback <- liftEffect $ Ref.read callbackRef
      for_ mbCallback \callback -> do
        callback value

    setCallback callback =
      liftEffect $ Ref.write callback callbackRef

  -- at some point in code, we trigger the callback via
  -- `pushValueAndTriggerCallback`

  -- Here, we set a callback that will handle the events emitted.
  -- We chose to use the `useTickEffect` version, but we could
  -- could have used the `useLifecycleEffect`
  Hooks.captures { state } useTickEffect do
    setCallback $ Just \i -> do
      -- here, we handle the event emitted
      liftEffect $ log $ "New value is: " <> show i

      -- here, we can set up some resources and do various things
      -- (code is not shown; use your imagination)

      -- here, we store the code necessary for unsubscribing
      let unsubscribeCode = pure unit -- unsubscribe
      mbUnsubscribe <- liftEffect $ Ref.read unsubscribeRef
      case mbUnsubscribe of
        Nothing -> do
          -- This is the first time we're running this, so
          -- write the unsubscribe code to a Ref,
          -- so that we can run it later
          liftEffect $ Ref.write (Just unsubscribeCode) unsubscribeRef
        _ -> do
          -- no need to store unsubscriber because
          -- 1. it's already been stored (i.e. this is the 2+ run)
          -- 2. we didn't setup any resources that need to be cleaned up later
          --     (i.e. this is purely an event handler)
          pure unit

    pure $ Just do
      -- Here, we read the Ref to get back the unsubscribe code,
      -- run it, and then set the `Ref` back to `Nothing`,
      -- so that future subscribes will work.
      mbUnsubscribe <- liftEffect $ Ref.read unsubscribeRef
      case mbUnsubscribe of
        Just unsubscribe' -> do
          unsubscribe'
          liftEffect $ Ref.write Nothing unsubscribeRef
        _ -> do
          pure unit

  Hooks.pure $ HH.text "example"
