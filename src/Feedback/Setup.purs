module Feedback.Setup where

import Prelude

import Feedback.Acc (initialAcc)
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Res, Trigger, World, Acc)
import WAGS.Change (ichange)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Graph.Parameter (_on)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

setup :: forall proof. TriggeredScene Trigger World () -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
setup (TriggeredScene { world: { buffers: { loop } } }) = do
  ichange
    { lpfG: 0.33
    , bpfG: 0.33
    , hpfG: 0.33
    , mainFader: 1.0
    , buf: { buffer: loop, onOff: _on }
    }
  pure initialAcc