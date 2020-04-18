module Halogen.Hooks.Extra.Hooks.UseEvent
  ( useEvent
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
import Effect.Ref as Ref
import Halogen.Hooks (HookM, Hooked, UseRef, useRef)
import Halogen.Hooks as Hooks

newtype UseEvent slots output m a hooks =
  UseEvent
    (UseRef (Maybe ( (HookM slots output m Unit -> HookM slots output m Unit)
                   -> a
                   -> HookM slots output m Unit
                   ))
    (UseRef (Maybe (HookM slots output m Unit))
    hooks))

derive instance newtypeUseEvent :: Newtype (UseEvent slots output m a hooks) _

-- | Use `push` to push events into the handler.
-- | Use `setCallback` to either "subscribe" to events for the first time
-- | (i.e. `Just firstCallback`), "unsubscribe" after subscribing previously
-- | (i.e. `Nothing`), or "unsubscribe" the previous event handler
-- | and "resubscribe" to the same events via a new handler
-- | (i.e. `Just newCallback`).
type EventApi slots output m a =
  { push :: a -> HookM slots output m Unit
  , setCallback
      :: Maybe (  (  HookM slots output m Unit -- sets up the unsubscribe callback
                  -> HookM slots output m Unit
                  )
               -> a -- pushed value
               -> HookM slots output m Unit -- code that gets run
               )
               -> HookM slots output m Unit
  , unsubscribe :: HookM slots output m Unit -- unsubscribe code
  }

-- | Allows you to "push" events that occur inside a hook
-- | to a single handler outside of the hook. This allows the end user
-- | to use the full API returned by the hook when handling the event.
-- | Moreover, the end-user can set up resources on the first time
-- | the handler is run and unsubscribe when the finalizer is run.
-- |
-- | For example...
-- | ```
-- | -- let's say this is the end-user's Hook code
-- | onEvent <- useEvent
-- |
-- | -- Here, we'll inline the code for a hypothetical hook we found. This could
-- | -- be a hook provided by a library or something.
-- | { foo } <- Hooks.do
-- |
-- |   -- somewhere in the hypothetical hook, an event occurs
-- |   onEvent.push "user clicked foo"
-- |
-- |   pure { foo: "foo" } -- return value of the hook provided by the library
-- |
-- | Hooks.useLifecycleEffect do
-- |   onEvent.setCallback $ Just \unsubscribeCallback string -> do
-- |     -- handle the event
-- |     Hooks.raise ("Event occurred: " <> string)
-- |
-- |     -- Then, set up some resources that later need to be cleaned up
-- |
-- |     -- now define the code that will run when we 'unsubscribe' later
-- |     unsubscribeCallback do
-- |       -- code we need to run when unsubscribing
-- |       pure unit
-- |
-- |   pure $ Just do
-- |     -- unsubscribe to clean up resources
-- |     onEvent.unsubscribe
-- |
-- | state /\ tState <- useState 0
-- |
-- | -- If we don't need to unsubscribe, just ignore the argument
-- | Hooks.captures { state } Hooks.useTickEffect do
-- |   -- notice how the first argument is an underscore,
-- |   -- showing that we are ignoring the 'unsubscribeCallback' argument
-- |   someLib.onEvent2 \_ string -> do
-- |     -- handle the event
-- |     Hooks.raise ("Event occurred: " <> string)
-- |
-- |   pure Nothing -- no need to unsubscribe here
-- | ```
useEvent :: forall slots output m a hooks
   . MonadEffect m
  => Hooked slots output m hooks (UseEvent slots output m a hooks)
      { push :: a -> HookM slots output m Unit
      , setCallback :: Maybe ((HookM slots output m Unit -> HookM slots output m Unit) -> a -> HookM slots output m Unit) -> HookM slots output m Unit
      , unsubscribe :: HookM slots output m Unit
      }
useEvent = Hooks.wrap Hooks.do
  _ /\ unsubscribeRef <- useRef Nothing
  _ /\ callbackRef <- useRef Nothing

  let
    push :: a -> HookM slots output m Unit
    push value = do
      mbCallback <- liftEffect $ Ref.read callbackRef
      let
        setupUnsubscribeCallback = \unsubscribe' -> do
          mbUnsubscribe <- liftEffect $ Ref.read unsubscribeRef
          case mbUnsubscribe of
            Nothing -> do
              liftEffect $ Ref.write (Just unsubscribe') unsubscribeRef
            _ -> do
              -- no need to store unsubscriber because
              -- 1. it's already been stored
              -- 2. no one has subscribed to this yet
              pure unit
      for_ mbCallback \callback -> do
        callback setupUnsubscribeCallback value

    setCallback callback =
      liftEffect $ Ref.write callback callbackRef

    unsubscribe = do
      mbUnsubscribe <- liftEffect $ Ref.read unsubscribeRef
      case mbUnsubscribe of
        Just unsubscribe' -> do
          unsubscribe'
          liftEffect $ Ref.write Nothing unsubscribeRef
        _ -> do
          pure unit


  Hooks.pure { push, setCallback, unsubscribe }
