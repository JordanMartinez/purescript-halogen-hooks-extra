-- | This module exists to increase readability in custom hooks
-- | functions by providing type aliases for the type signatures
-- | of specific functions or values.
module Halogen.Hooks.Extra.TypeSignatures where

import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Halogen.Hooks (HookM, Hooked, MemoValues, UseEffect)

-- | The type signature for `Hooks.useTickEffect`.
type UseTickEffect slots output m =
  HookM slots output m (Maybe (HookM slots output m Unit))
  -> (forall hooks. Hooked slots output m hooks (UseEffect hooks) Unit)

-- | Type signature for the `eqFn` value in
-- | ```
-- | Hooks.capturesWith eqFn { state } Hooks.useTickEffect do
-- |   someHookMAction
-- | ```
type CapturesWithEqFn rows = { | rows } -> { | rows } -> Boolean

-- | Type signatures for `Hooks.capturesWith`.
type CapturesWith slots output m rows =
  CapturesWithEqFn rows
    -> (MemoValues -> UseTickEffect slots output m)
    -> UseTickEffect slots output m
