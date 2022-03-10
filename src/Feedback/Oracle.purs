module Feedback.Oracle where

import Prelude

import Data.Functor (voidRight)
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Slider(..), Res, Trigger(..), World)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Graph.Parameter (AudioSingleNumber(..), _linearRamp, singleNumber)
import WAGS.Math (calcSlope)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle
  ( TriggeredScene
      { trigger }
  )
  a = do
  case trigger of
    Thunk -> pure a
    Slider slider -> voidRight a case slider of
      Slider0 param -> ichange' (Proxy :: _ "lpfF")
        { freq: singleNumber
            ( AudioSingleNumber
                { param: calcSlope 0.0 600.0 1.0 50.0 param
                , timeOffset: 0.05
                , transition: _linearRamp
                }
            )
        }
      Slider1 param -> ichange' (Proxy :: _ "lpfF")
        { q: singleNumber
            ( AudioSingleNumber
                { param: calcSlope 0.0 0.5 1.0 40.0 param
                , timeOffset: 0.05
                , transition: _linearRamp
                }
            )
        }
      Slider2 param -> ichange' (Proxy :: _ "bpfF")
        { freq: singleNumber
            ( AudioSingleNumber
                { param: calcSlope 0.0 200.0 1.0 800.0 param
                , timeOffset: 0.05
                , transition: _linearRamp
                }
            )
        }
      Slider3 param -> ichange' (Proxy :: _ "bpfF")
        { q: singleNumber
            ( AudioSingleNumber
                { param: calcSlope 0.0 0.5 1.0 40.0 param
                , timeOffset: 0.05
                , transition: _linearRamp
                }
            )
        }
      Slider4 param -> ichange' (Proxy :: _ "hpfF")
        { freq: singleNumber
            ( AudioSingleNumber
                { param: calcSlope 0.0 1000.0 1.0 4000.0 param
                , timeOffset: 0.05
                , transition: _linearRamp
                }
            )
        }
      Slider5 param -> ichange' (Proxy :: _ "hpfF")
        { q: singleNumber
            ( AudioSingleNumber
                { param: calcSlope 0.0 0.5 1.0 40.0 param
                , timeOffset: 0.05
                , transition: _linearRamp
                }
            )
        }
