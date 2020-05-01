module Halogen.Hooks.Extra.Hooks.UseDebouncer
  ( useDebouncer
  , UseDebouncer
  )
  where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Fiber, Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (HookM, Hook, UseRef)
import Halogen.Hooks as Hooks

newtype UseDebouncer a hooks =
  UseDebouncer (UseRef (Maybe a) (UseRef (Maybe Debouncer) hooks))

derive instance newtypeUseDebouncer :: Newtype (UseDebouncer a hooks) _

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

-- | A hook that, once the given time period ends, will run an action using
-- | the last value written. Once the initial write occurs, a timer is set
-- | and begins counting down. If a new write occurs before that timer ends,
-- | the timer restarts. When the timer ends, the last value that was written
-- | will be passed into the handler.
-- |
-- | ## Example Usage
-- |
-- | The below example shows how to update the label with the value the
-- | user inputted after there have been 500ms of no user input.
-- |
-- | ```
-- | myComponent = Hooks.component \_ -> Hooks.do
-- |   label /\ tLabel <- useState ""
-- |   makeNewSearchFor <- useDebouncer (Milliseconds 500.0) \finalValue -> do
-- |      Hooks.put tLabel finalValue
-- |
-- |   Hooks.pure
-- |    HH.div_
-- |      [ HH.h1_
-- |        [ HH.text $ "Label text is: " <> label ]
-- |      , HH.input
-- |        [ HP.onValueInput \str -> Just (makeNewSearchFor str) ]
-- |      ]
-- | ```
useDebouncer
  :: forall m a
   . MonadAff m
  => Milliseconds
  -> (a -> HookM m Unit)
  -> Hook m (UseDebouncer a) (a -> HookM m Unit)
useDebouncer ms fn = Hooks.wrap Hooks.do
  _ /\ debounceRef <- Hooks.useRef Nothing
  _ /\ valRef <- Hooks.useRef Nothing

  let
    debounceFn x = do
      debouncer <- liftEffect do
        Ref.write (Just x) valRef
        Ref.read debounceRef

      case debouncer of
        Nothing -> do
          var <- liftAff AVar.empty
          fiber <- liftAff $ forkAff do
            delay ms
            AVar.put unit var

          _ <- Hooks.fork do
            _ <- liftAff $ AVar.take var
            val <- liftEffect do
              Ref.write Nothing debounceRef
              Ref.read valRef
            traverse_ fn val

          liftEffect do
            Ref.write (Just { var, fiber }) debounceRef

        Just db -> do
          let var = db.var
          fiber <- liftAff do
            killFiber (error "Time's up!") db.fiber
            forkAff do
              delay ms
              AVar.put unit var

          liftEffect $ Ref.write (Just { var, fiber }) debounceRef

  Hooks.pure debounceFn
