-- | Reexports all Hooks defined in this repository.
module Halogen.Hooks.Extra.Hooks
  ( module Exports
  ) where

import Halogen.Hooks.Extra.Hooks.UseDebouncer (UseDebouncer, useDebouncer) as Exports
import Halogen.Hooks.Extra.Hooks.UseEvent (UseEvent, UseEventApi, useEvent) as Exports
import Halogen.Hooks.Extra.Hooks.UseGet (UseGet, useGet) as Exports
import Halogen.Hooks.Extra.Hooks.UseStateFn (UseStateFn, useStateFn, useModifyState_, useModifyState, usePutState) as Exports
import Halogen.Hooks.Extra.Hooks.UseThrottle (UseThrottle, useThrottle) as Exports
