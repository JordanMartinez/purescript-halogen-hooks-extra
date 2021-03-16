module Halogen.Hooks.Extra.Hooks.UseDebouncer
  ( useDebouncer
  , UseDebouncer
  )
  where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Fiber, Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (HookM, Hook, UseRef, type (<>), class HookNewtype)
import Halogen.Hooks as Hooks

type UseDebouncer' a =
  UseRef { debounce :: Maybe Debouncer, val :: Maybe a } <> Hooks.Pure
foreign import data UseDebouncer :: Type -> Hooks.HookType
instance hooknewtypeUseDebouncer :: HookNewtype (UseDebouncer a) (UseDebouncer' a)

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
-- |   label /\ labelId <- useState ""
-- |   makeNewSearchFor <- useDebouncer (Milliseconds 500.0) \finalValue -> do
-- |      Hooks.put labelId finalValue
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
useDebouncer ms fn = Hooks.wrap hook
  where
  hook :: Hook m (UseDebouncer' a) (a -> HookM m Unit)
  hook = Hooks.do
    _ /\ ref <- Hooks.useRef { debounce: Nothing, val: Nothing }

    let
      debounceFn x = do
        debouncer <- liftEffect do
          map (_.debounce) $ Ref.modify (\s -> s { val = Just x }) ref

        case debouncer of
          Nothing -> do
            var <- liftAff AVar.empty
            fiber <- liftAff $ forkAff do
              delay ms
              AVar.put unit var

            _ <- Hooks.fork do
              _ <- liftAff $ AVar.take var
              val <- liftEffect do
                map (_.val) $ Ref.modify (\s -> s { debounce = Nothing}) ref
              traverse_ fn val

            liftEffect do
              Ref.modify_ (\s -> s { debounce = Just { var, fiber }}) ref

          Just db -> do
            let var = db.var
            fiber <- liftAff do
              killFiber (error "Time's up!") db.fiber
              forkAff do
                delay ms
                AVar.put unit var

            liftEffect $ Ref.modify_ (\s -> s { debounce = Just { var, fiber }}) ref

    Hooks.pure debounceFn
