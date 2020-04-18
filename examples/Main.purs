module Examples.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Examples.UseEvent.UseLTEffectHandler as UseEvent
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI topComponent unit body
  where
    topComponent = Hooks.component \_ -> Hooks.pure render

    render =
      HH.div_
        [ renderExample "useEvent" _useEvent UseEvent.component
        ]


    renderExample labelName sproxy comp =
      HH.div_
        [ HH.h2_ [ HH.text labelName ]
        , HH.slot sproxy unit comp unit (const Nothing)
        ]

    -- sproxies
    _useEvent = SProxy :: SProxy "useEvent"
