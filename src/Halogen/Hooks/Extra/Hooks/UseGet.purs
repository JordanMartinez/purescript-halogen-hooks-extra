-- | Idea and implementation by Thomas Honeyman. This was copied
-- | over from the Halogen Hooks repository's Examples folder.
module Halogen.Hooks.Extra.Hooks.UseGet
  ( useGet
  , UseGet
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (Hook, HookM, UseEffect, UseRef, type (<>), class HookNewtype)
import Halogen.Hooks as Hooks

type UseGet' a = UseRef a <> UseEffect <> Hooks.Pure
foreign import data UseGet :: Type -> Hooks.HookType
instance hooknewtypeUseGet :: HookNewtype (UseGet a) (UseGet' a)

-- | Use this hook when you wish to ensure that your reference to a state
-- | value or the component's input is not "stale" or outdated. Usually, this
-- | happens in when you define a computation in one "Hook evaluation cycle,"
-- | but you do not run the computation until a future "Hook evaluation cycle."
-- | This typically occurs when running a `useLifecycleEffect`/`useTickEffect`'s
-- | cleanup/finalizer/unsubscribe computation.
-- |
-- | Let's see an example of an effect's finalizer referring to a stale value
-- | in code. If you don't use `useGet` in this situation, you will refer to
-- | what the value used to be (a stale value), not what the value is now:
-- | ```
-- | myComponent :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
-- | myComponent = Hooks.component \_ _ -> Hooks.do
-- |   thisIsFive_NotSix /\ stateId <- Hooks.useState 5
-- |   let modifyState = Hooks.modify_ stateId
-- |
-- |   Hooks.captures {} Hooks.useTickEffect do
-- |     -- The `thisIsFive_NotSix` state reference is currently `5` and
-- |     -- is up-to-date because this effect body runs immediately
-- |     -- after the Hook evaluation in which it is defined.
-- |     -- Thus, this will print "5" to the console.
-- |     logShow thisIsFive_NotSix
-- |
-- |     -- Now we change the value to 6
-- |     modifyState (_ + 1)
-- |
-- |     pure $ Just $ do
-- |       -- The effect cleanup, however, will not run after the Hook
-- |       -- evaluation in which it is defined. Thus, the `thisIsFive_NotSix`
-- |       -- state reference is still `5` even though we previously
-- |       -- updated the real value to 6.
-- |       -- Thus, this will print "5" to the console when it should print "6".
-- |       logShow thisIsFive_NotSix
-- | ```
-- | To ensure we refer to the latest value and not a stale one, we use
-- | this hook to do so.
-- | ```
-- | myComponent :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
-- | myComponent = Hooks.component \_ _ -> Hooks.do
-- |   thisIsFive_NotSix /\ stateId <- Hooks.useState 5
-- |   let modifyState = Hooks.modify_ stateId
-- |
-- |   -- This returns a function to get the latest state/input value.
-- |   getState <- useGet thisIsFive_NotSix
-- |
-- |   Hooks.captures {} Hooks.useTickEffect do
-- |     logShow thisIsFive_NotSix
-- |
-- |     modifyState (_ + 1)
-- |
-- |     pure $ Just $ do
-- |       -- Now we get the latest value rather than using the stale value.
-- |       -- This correctly prints "6".
-- |       thisIsSix_NotFive <- getState
-- |       logShow thisIsSix_NotFive
-- | ```
useGet
  :: forall m a
   . MonadEffect m
  => a
  -> Hook m (UseGet a) (HookM m a)
useGet latest = Hooks.wrap hook
  where
  hook :: Hook m (UseGet' a) (HookM m a)
  hook = Hooks.do
    _ /\ ref <- Hooks.useRef latest

    Hooks.captures {} Hooks.useTickEffect do
      liftEffect $ Ref.write latest ref
      pure Nothing

    Hooks.pure (liftEffect $ Ref.read ref)
