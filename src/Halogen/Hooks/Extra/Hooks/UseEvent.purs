module Halogen.Hooks.Extra.Hooks.UseEvent
  ( useEvent
  , UseEvent
  , UseEventApi
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (Hook, HookM, UseRef, useRef, type (<>), class HookNewtype)
import Halogen.Hooks as Hooks

type UseEvent' m a =
  UseRef
    { valueCB
      :: Maybe (  ((HookM m (HookM m Unit)) -> HookM m Unit)
               -> a -- pushed value
               -> HookM m Unit -- code that gets run
               )
    , unsubscribeCB
      :: Maybe (HookM m Unit)
    }
  <> Hooks.Pure
foreign import data UseEvent :: (Type -> Type) -> Type -> Hooks.HookType
instance hooknewtypeUseEvent :: HookNewtype (UseEvent m a) (UseEvent' m a)

-- | For proper usage, see the docs for `useEvent`.
type UseEventApi m a =
  { push :: a -> HookM m Unit
  , setCallback
      :: Maybe
           (  ((HookM m (HookM m Unit)) -> HookM m Unit)
           -> a -- pushed value
           -> HookM m Unit -- code that gets run
           )
           -> HookM m (HookM m Unit)
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
-- | -- Here, we'll inline the code for a hypothetical hook we found.
-- | -- This could be a hook provided by a library or something.
-- | { foo } <- Hooks.do
-- |
-- |   -- somewhere in the hypothetical hook, an event occurs
-- |   onEvent.push "user clicked foo"
-- |
-- |   -- return the value of the hook provided by the library
-- |   pure { foo: "foo" }
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
-- | ```
-- | If you don't need to unsubscribe, just ignore the first argument
-- | in the function passed to `onEvent`:
-- | ```
-- | state /\ stateId <- useState 0
-- |
-- | Hooks.captures { state } Hooks.useTickEffect do
-- |   -- Notice two things here:
-- |   -- 1. we're ignoring the 'unsubscribeCallback' argument
-- |   --        by using the underscore (i.e. _).
-- |   -- 2. we're ignoring the returned 'unsubscribe' code by using `void`.
-- |   void $ onEvent \_ string -> do
-- |     -- handle the event
-- |     Hooks.raise ("Event occurred: " <> string)
-- |
-- |   pure Nothing -- no need to unsubscribe here
-- | ```
-- |
-- | ## Beware Infinite Loops
-- |
-- | If you use this hook, it's possible for you to create an infinite loop.
-- | This will occur if a handler runs code that causes another event to
-- | be emitted. Consider this workflow:
-- |
-- | 1. Event A is emitted
-- | 2. During A's handler, Action X is called
-- | 3. During Action X's computation, Event A is emitted.
-- | 4. An infinite loop occurs (go to Step 2)
-- |
-- | Here's an example in code:
-- | ```
-- | library <- useLibrary
-- | useLifecycleEffect do
-- |   library.onNewInteger \newInt -> do
-- |     library.setIntValue (newInt + 1)
-- | ```
-- | Consider also cases where the chain is longer and
-- | some computations run only when certain conditions are true:
-- |
-- | 1. Event A is emitted
-- | 2. During A's handler, Action X is called
-- | 3. During Action X's computation, Event B is emitted.
-- | 4. During B's handler, Action Y is called
-- | 5. During Action Y's computation, Event C is emitted but only if State M is equal to 4.
-- | 6. During C's handler, Action Z is called
-- | 7. During Action Z's computation, Event A is emitted.
-- | 8. Infinite loop may occur (depends on State M)
useEvent :: forall m a
   . MonadEffect m
  => Hook m (UseEvent m a) (UseEventApi m a)
useEvent = Hooks.wrap hook
  where
  hook :: Hook m (UseEvent' m a) (UseEventApi m a)
  hook = Hooks.do
    -- valueCB = the callback to run when a new event is pushed
    -- unsubscribeCB = callback to run when unsubscribing
    _ /\ ref <- useRef { valueCB: Nothing, unsubscribeCB: Nothing }

    let
      push :: a -> HookM m Unit
      push value = do
        mbCallback <- liftEffect $ map (_.valueCB) $ Ref.read ref
        for_ mbCallback \callback -> do
          callback setupUnsubscribeCallback value

      setupUnsubscribeCallback :: (HookM m (HookM m Unit)) -> HookM m Unit
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
               (  ((HookM m (HookM m Unit)) -> HookM m Unit)
               -> a -- pushed value
               -> HookM m Unit -- code that gets run
               )
               -> HookM m (HookM m Unit)
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
