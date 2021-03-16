module Halogen.Hooks.Extra.Hooks.UseThrottle
  ( useThrottle
  , UseThrottle
  )
  where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (HookM, Hook, UseRef, type (<>), class HookNewtype)
import Halogen.Hooks as Hooks

type UseThrottle' a = UseRef { running :: Boolean, val :: Maybe a } <> Hooks.Pure
foreign import data UseThrottle :: Type -> Hooks.HookType
instance hooknewtypeUseThrottle :: HookNewtype (UseThrottle a) (UseThrottle' a)

-- | Limits the amount of times an action is performed per time period.
-- | Use this hook when you need to run the same action repeatedly with a different input,
-- | but you are concerned about performance or resource usage.
-- |
-- | ## Example Usage
-- |
-- | The below example shows how to update the label with the mouse position,
-- | limiting the number of times the label is updated to once every 100ms.
-- |
-- | ```
-- | myComponent = Hooks.component \_ _ -> Hooks.do
-- |   position /\ positionId <- useState { x: zero, y: zero }
-- |   throttledMouseMove <- useThrottle (Milliseconds 100.0) \e -> do
-- |     Hooks.modify_ positionId (_ { x = MouseEvent.pageX e, y = MouseEvent.pageY e}))
-- |
-- |   Hooks.pure $
-- |     HH.div
-- |       [ HE.onMouseMove $ Just <<< throttledMouseMove ]
-- |       [ HH.label_
-- |         [ HH.text $ "Mouse position: (" <>
-- |              show position.x <> ", " <> show position.y <> ")"
-- |         ]
-- |       ]
-- | ```
useThrottle
  :: forall m a
   . MonadAff m
  => Milliseconds
  -> (a -> HookM m Unit)
  -> Hook m (UseThrottle a) (a -> HookM m Unit)
useThrottle ms fn = Hooks.wrap hook
  where
  hook :: Hook m (UseThrottle' a) (a -> HookM m Unit)
  hook = Hooks.do
    _ /\ ref <- Hooks.useRef { running: false, val: Nothing }

    let
      throttleFn x = do
        running <- liftEffect $
          map (_.running) $ Ref.modify (_ { val = Just x }) ref

        unless running do
          void $ Hooks.fork do
            liftAff $ delay ms
            val <- liftEffect $
              map (_.val) $ Ref.modify (_ { running = false}) ref
            traverse_ fn val

          liftEffect $
            Ref.modify_ (_ { running = true }) ref

    Hooks.pure throttleFn
