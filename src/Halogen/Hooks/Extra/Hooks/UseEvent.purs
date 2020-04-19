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
    (UseRef
      { valueCB
        :: Maybe (  ((HookM slots output m (HookM slots output m Unit)) -> HookM slots output m Unit)
                 -> a -- pushed value
                 -> HookM slots output m Unit -- code that gets run
                 )
      , unsubscribeCB
        :: Maybe (HookM slots output m Unit)
      }
    hooks)

derive instance newtypeUseEvent :: Newtype (UseEvent slots output m a hooks) _

-- | For proper usage, see the docs for `useEvent`.
type EventApi slots output m a =
  { push :: a -> HookM slots output m Unit
  , setCallback
      :: Maybe
           (  ((HookM slots output m (HookM slots output m Unit)) -> HookM slots output m Unit)
           -> a -- pushed value
           -> HookM slots output m Unit -- code that gets run
           )
           -> HookM slots output m (HookM slots output m Unit)
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
-- |   unsubscribe <- onEvent.setCallback $ Just \setupSubscription str -> do
-- |     -- handle the event
-- |     Hooks.raise ("Event occurred: " <> str)
-- |
-- |     setupSubscription do
-- |       -- Then, set up some resources in this code block
-- |       -- that need to be cleaned up later
-- |       liftEffect $ log $ "Setting up resources."
-- |
-- |       pure do
-- |         -- now define the code that will run when we call
-- |         -- 'unsubscribe' later
-- |         liftEffect $ log $ "Cleaning up resources."
-- |
-- |   pure $ Just do
-- |     -- unsubscribe to clean up resources
-- |     unsubscribe
-- |
-- | state /\ tState <- useState 0
-- |
-- | -- If we don't need to unsubscribe, just ignore the argument
-- | Hooks.captures { state } Hooks.useTickEffect do
-- |   -- notice two things here. First, we're ignoring the
-- |   -- 'unsubscribeCallback' argument by using the underscore (i.e. _)
-- |   -- Second, we're ignoring the returned 'unsubscribe' code by using
-- |   -- `void`.
-- |   void $ onEvent \_ string -> do
-- |     -- handle the event
-- |     Hooks.raise ("Event occurred: " <> string)
-- |
-- |   pure Nothing -- no need to unsubscribe here
-- | ```
useEvent :: forall slots output m a hooks
   . MonadEffect m
  => Hooked slots output m hooks (UseEvent slots output m a hooks)
      (EventApi slots output m a)
useEvent = Hooks.wrap Hooks.do
  -- valueCB = the callback to run when a new event is pushed
  -- unsubscribeCB = callback to run when unsubscribing
  _ /\ ref <- useRef { valueCB: Nothing, unsubscribeCB: Nothing }

  let
    push :: a -> HookM slots output m Unit
    push value = do
      mbCallback <- liftEffect $ map (_.valueCB) $ Ref.read ref
      for_ mbCallback \callback -> do
        callback setupUnsubscribeCallback value

    setupUnsubscribeCallback :: (HookM slots output m (HookM slots output m Unit)) -> HookM slots output m Unit
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
             (  ((HookM slots output m (HookM slots output m Unit)) -> HookM slots output m Unit)
             -> a -- pushed value
             -> HookM slots output m Unit -- code that gets run
             )
             -> HookM slots output m (HookM slots output m Unit)
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

  Hooks.pure { push, setCallback }
