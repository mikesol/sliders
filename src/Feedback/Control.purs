module Feedback.Control where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Feedback.Types (Slider)
import WAGS.WebAPI (AudioContext)

data Action
  = StartAudio
  | StopAudio
  | SliderFired Slider

type State =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  }
