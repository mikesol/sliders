module Feedback.Types where


import WAGS.WebAPI (BrowserAudioBuffer)

type Buffers = { loop :: BrowserAudioBuffer }

data Trigger
  = Thunk
  | Slider Slider

data Slider
  = Slider0 Number
  | Slider1 Number
  | Slider2 Number
  | Slider3 Number
  | Slider4 Number
  | Slider5 Number

type World = { buffers :: Buffers }

type Res = {}

type Acc = {}