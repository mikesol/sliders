module Feedback.App where

import Prelude

import Control.Parallel (parallel, sequential)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (EventIO)
import Feedback.InnerComponent as InnerComponent
import Feedback.Types (Buffers, Slider)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import WAGS.Interpret (context, close, decodeAudioDataFromUri)

type State = { buffers :: Maybe Buffers }

data Action = Initialize

component :: forall query input output m. MonadEffect m => MonadAff m => EventIO Slider -> H.Component query input output m
component slider =
  H.mkComponent
    { initialState
    , render: render slider
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

type Slots = (canvas :: forall query. H.Slot query Void Unit)
_canvas = Proxy :: Proxy "canvas"

initialState :: forall input. input -> State
initialState _ = { buffers: Nothing }

klz :: forall r a. Array String -> IProp (class :: String | r) a
klz = HP.classes <<< map ClassName

render :: forall m. MonadEffect m => MonadAff m => EventIO Slider -> State -> H.ComponentHTML Action Slots m
render slider { buffers } =
  HH.div [ klz [ "w-screen", "h-screen" ] ] $
    maybe
      [ HH.div [ klz [ "flex", "flex-col", "w-full", "h-full" ] ]
          [ HH.div [ klz [ "flex-grow" ] ] [ HH.div_ [] ]
          , HH.div [ klz [ "flex-grow-0", "flex", "flex-row" ] ]
              [ HH.div [ klz [ "flex-grow" ] ]
                  []
              , HH.div [ klz [ "flex", "flex-col" ] ]
                  [ HH.h1 [ klz [ "text-center", "text-3xl", "font-bold" ] ]
                      [ HH.text "Loading..." ]
                  ]
              , HH.div [ klz [ "flex-grow" ] ] []
              ]
          , HH.div [ klz [ "flex-grow" ] ] []
          ]
      ]
      ( append []
          <<< pure
          <<< flip (HH.slot_ _canvas unit) unit
          <<< InnerComponent.component slider
      )
      buffers

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    audioCtx <- H.liftEffect context
    buffers <-
      H.liftAff
        $ sequential
        $
          ( map fromHomogeneous
              $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
              $ homogeneous
                  { loop: "https://freesound.org/data/previews/216/216624_3162775-hq.mp3"
                  }
          )
    H.liftEffect $ close audioCtx
    H.modify_
      ( _
          { buffers = Just buffers }
      )
