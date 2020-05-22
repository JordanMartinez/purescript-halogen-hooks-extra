module Examples.UseThrottle where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks.UseThrottle (useThrottle)
import Web.UIEvent.MouseEvent as MouseEvent

component :: H.Component HH.HTML (Const Void) Unit Unit Aff
component = Hooks.component \_ _ -> Hooks.do
  position /\ modifyPosition <- useState { x: zero, y: zero }
  throttling /\ modifyThrottling <- useState false

  let
    mouseMoveHandler e = modifyPosition (_ { x = MouseEvent.pageX e, y = MouseEvent.pageY e})

  throttledMouseMove <- useThrottle (Milliseconds 250.0) mouseMoveHandler

  Hooks.pure $
    HH.div
      [ HE.onMouseMove $ (Just <<< if throttling then throttledMouseMove else mouseMoveHandler) ]
      [
        HH.p_ [ HH.text "Move the mouse cursor over", HH.br_, HH.text "this text to update its position below." ]
      , HH.label_ [ HH.text $ "Mouse position: (" <> show position.x <> ", " <> show position.y <> ")" ]
      , HH.br_
      , HH.button
          [ HE.onClick $ Just <<< (const $ modifyThrottling not) ]
          [ HH.text "Click to toggle throttling" ]
    ]
