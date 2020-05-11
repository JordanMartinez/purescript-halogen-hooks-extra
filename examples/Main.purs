module Examples.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Examples.UseEvent.UseLTEffectHandler as UseEvent
import Examples.UseThrottle.MouseMoveHandler as UseThrottle
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI topComponent unit body
  where
    topComponent = Hooks.component \_ _ -> Hooks.pure render

    render =
      HH.div_
        [ renderExample "useEvent" $ HH.slot _useEvent unit UseEvent.component unit (const Nothing)
        , renderExample "useThrottle" $ HH.slot _useThrottle unit UseThrottle.component unit (const Nothing)
        ]

    renderExample label html =
      HH.div_
        [ HH.h2_ [ HH.text label ]
        , html
        ]

    -- sproxies
    _useEvent = SProxy :: SProxy "useEvent"
    _useThrottle = SProxy :: SProxy "useThrottle"
