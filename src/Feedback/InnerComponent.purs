module Feedback.InnerComponent where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (EventIO, subscribe)
import Feedback.Control (Action(..), State)
import Feedback.Engine (piece)
import Feedback.Oracle (oracle)
import Feedback.Setup (setup)
import Feedback.Types (Buffers, Slider(..), Res, Trigger(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Run (TriggeredRun, runNoLoop)
import Web.Event.Internal.Types (Event)

component :: forall query input output m. MonadEffect m => MonadAff m => EventIO Slider -> Buffers -> H.Component query input output m
component sliders buffers =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction sliders buffers
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

foreign import sliderValue :: Event -> Number

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [ classes [ "w-full", "h-full" ] ]
  [ HH.div [ classes [ "flex", "flex-row", "w-full", "h-full" ] ]
      [ HH.div [ classes [ "flex-grow" ] ] [ HH.div_ [] ]
      , HH.div [ classes [ "flex-grow-0", "flex", "flex-col" ] ]
          [ HH.div [ classes [ "flex-grow" ] ]
              []
          , HH.div [ classes [ "flex" ] ]
              let
                f x = HH.p []
                  [ HH.input
                      [ HP.min 0.0
                      , HP.max 100.0
                      , HP.type_ HP.InputRange
                      , HE.onInput (sliderValue >>> x >>> SliderFired)
                      , classes ["m-5"]
                      ]
                  ]
                g x y = HH.div_ [HH.p_ [HH.text x], f y ]
              in
                [ g "LPF freq" Slider0
                , g "LPF q" Slider1
                , g "BPF freq" Slider2
                , g "BPF q" Slider3
                , g "HPF freq" Slider4
                , g "HPF q" Slider5
                ]
          , HH.div [ classes [ "flex-grow" ] ]
              case st.audioCtx of
                Nothing ->
                  [ HH.button
                      [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                      [ HH.text "Start audio" ]
                  ]
                Just _ ->
                  [ HH.button
                      [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StopAudio ]
                      [ HH.text "Stop audio" ]
                  ]
          ]
      , HH.div [ classes [ "flex-grow" ] ] []
      ]
  ]

handleAction :: forall output m. MonadEffect m => MonadAff m => EventIO Slider -> Buffers -> Action -> H.HalogenM State Action () output m Unit
handleAction sliders buffers = case _ of
  StartAudio -> do
    handleAction sliders buffers StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop
                (pure Thunk <|> Slider <$> sliders.event)
                (pure { buffers })
                {}
                ffiAudio
                (piece setup oracle)
            )
            (\(_ :: TriggeredRun Res ()) -> pure unit)
    H.modify_ _
      { unsubscribe = unsubscribe
      , audioCtx = Just audioCtx
      }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    H.liftEffect do
      for_ audioCtx close
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
  SliderFired k -> do
    H.liftEffect $ sliders.push k
