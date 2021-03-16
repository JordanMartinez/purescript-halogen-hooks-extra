module Examples.Main where

import Prelude

import Type.Proxy (Proxy(..))
import Effect (Effect)
import Examples.UseEvent as UseEvent
import Examples.UseThrottle as UseThrottle
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
        [ renderExample "useEvent" $ HH.slot_ _useEvent unit UseEvent.component unit
        , renderExample "useThrottle" $ HH.slot_ _useThrottle unit UseThrottle.component unit
        ]

    renderExample label html =
      HH.div_
        [ HH.h2_ [ HH.text label ]
        , html
        ]

    -- sproxies
    _useEvent = Proxy :: Proxy "useEvent"
    _useThrottle = Proxy :: Proxy "useThrottle"
