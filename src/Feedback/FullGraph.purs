module Feedback.FullGraph where

import Prelude

import Data.Tuple.Nested (type (/\))
import WAGS.Graph.AudioUnit (TGain, TLoopBuf, TLowpass, TSpeaker)

type FullGraph =
  ( speaker :: TSpeaker /\ { mainFader :: Unit }
  , mainFader ::
      TGain /\
        { lpfG :: Unit
        , bpfG :: Unit
        , hpfG :: Unit
        }
  , lpfG :: TGain /\ { lpfF :: Unit }
  , lpfF :: TLowpass /\ { buf :: Unit }
  , bpfG :: TGain /\ { bpfF :: Unit }
  , bpfF :: TLowpass /\ { buf :: Unit }
  , hpfG :: TGain /\ { hpfF :: Unit }
  , hpfF :: TLowpass /\ { buf :: Unit }
  , buf :: TLoopBuf /\ {}
  )
