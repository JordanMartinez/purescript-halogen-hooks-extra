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
-- |
-- | There are two ways this can be used, depending on what the value of
-- | the type, `a`, is. The first is more flexible but uses multiple
-- | `useRef` hooks internally whereas the second is less flexible but
-- | more performant because only one `useRef` hook is used internally.
-- |
-- | Note: the event handler is a callback that is stored in a `Ref`. Thus, you
-- | can "unsubscribe" from the events by setting the `Ref`'s value to `Nothing`
-- | or you can "unsubscribe" and "resubscribe" in the same action by setting
-- | the `Ref`'s value to `Just newCallback`.
-- |
-- |
-- | ## Conditionally Subscribe and Unsubscribe (Less Performant)
-- |
-- | In these kinds of situations, one would `useEvent` one time for each event.
-- | One should use this approach when one needs to conditionally subscribe
-- | and unsubscribe to a hook's internal events. For example, when X is true,
-- | then subscribe to the event that notifies us that some aspect of our code
-- | is now visible. When X is false, don't get notified of those things
-- | anymore.
-- | Or, perhaps you wish to change how you handle those events. When X is true,
-- | do Y. When X is false, do Z. Use this hook according to the below example
-- | in such circumstances:
-- |
-- | ```
-- | -- in the custom Hook code...
-- | onEvenUnit <- useEvent
-- | onEvenHookM slots output m <- useEvent
-- |
-- | -- somewhere in your HookM code
-- |   onEvenUnit.push "user clicked foo"
-- |
-- | -- somewhere else in your HookM code
-- |   onEvenHookM slots output m.push "user clicked foo"
-- |
-- | Hooks.pure
-- |   { onEvenUnit: onEvenUnit.setCallback
-- |   , onEvenHookM slots output m: onEvenHookM slots output m.setCallback
-- |   }
-- |
-- | --------------
-- | -- in end user Hook code
-- |
-- | state /\ tState <- useState 0
-- | someLib <- useSomeLibHook
-- |
-- | Hooks.captures { state } Hooks.useTickEffect do
-- |   someLib.onEvenUnit \string -> do
-- |     Hooks.raise ("Event occurred: " <> string)
-- | Hooks.captures { state, state2 } Hooks.useTickEffect do
-- |   state1' <- Hooks.get tState1
-- |   state2' <- Hooks.get tState2
-- |   when (state1 /= state2) do
-- |     someLib.onEvenHookM slots output m \string -> do -- something
-- | ```
-- |
-- | ## Subscribe on Initialization and Never Unsubscribe (More Performant)
-- |
-- | In this situation, one would `useEvent` one time for the entire
-- | component to which multiple hooks can push their specific events.
-- | One should use this when they will always subscribe to these events
-- | using the same event handlers. In other words, they won't need to
-- | unsubscribe and resubscribe later on.
-- | Use this hook according to the below example in such circumstances:
-- | ```
-- | -- in the custom Hook code...
-- | useFoo pushEvent = Hooks.do
-- |
-- |  -- somewhere in your HookM code
-- |   pushEvent "user clicked foo"
-- |
-- | Hooks.pure customHookReturnValue
-- |
-- | --------------
-- | -- in end user Hook code
-- |
-- | eventBus <- useEvent
-- |
-- | let
-- |   _libName = SProxy :: SProxy "libName"
-- |   pushLibEvent = \value -> eventBus.push $ Variant.inj _libName value
-- |
-- | someLib <- useSomeLibHook pushLibEvent
-- | useLifecycleEffect do
-- |   eventBus.setCallback $
-- |     Variant.case_
-- |       # _libName \string -> do
-- |            Hooks.raise ("Event occurred: " <> string)
-- |       # _otherLibName \x -> do
-- |           Hooks.put tMyState x
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
