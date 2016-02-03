module App where

import Graphics.Element exposing (Element)
import Reactive exposing (Reactive, Event(..), TouchType(..), Navigation(..))
import Pic exposing (Pic)
import ExtraSignals
import Window
import Text

type alias App model message =
  { init : model
  , update : message -> model -> model
  , view : model -> Reactive message
  }

-- Running Apps

run : App model message -> Signal Element
run { init, update, view } =
  let
    updateState newModel navigation =
      (newModel, view newModel, navigation)

    updateReactive event (model, reactive, oldNavigation) =
      let
        navigation = maybeNewNavigation event reactive oldNavigation
       in case reactive.reaction event navigation of
            Just message ->
              updateState (update message model) navigation

            Nothing ->
              (model, reactive, navigation)

    maybeNewNavigation event reactive navigation =
      case event of
        (TouchEvent FingerDown pos) -> reactive.pick pos
        otherwise -> navigation

    makeElement size (_, { visual }, nav) =
      Pic.toElement size (Pic.nextTo Pic.Up (Pic.debugEnvelope visual) (Pic.debugEnvelope <| Pic.text <| Text.fromString <| toString nav))
  in
    Signal.map2 makeElement Window.dimensions
    <| Signal.foldp updateReactive (init, view init, Nowhere) (eventsSignal Window.dimensions)

eventsSignal : Signal (Int, Int) -> Signal Event
eventsSignal collageSize =
  let fixedPosition = Signal.map2 fixPosition collageSize ExtraSignals.fingerPosition
   in Signal.mergeMany
        [ Signal.map (TouchEvent FingerDown) (Signal.sampleOn ExtraSignals.fingerPressed fixedPosition)
        , Signal.map (TouchEvent FingerUp) (Signal.sampleOn ExtraSignals.fingerReleased fixedPosition)
        , Signal.map (TouchEvent FingerMove) fixedPosition
        ]

-- turns event-space coordinates to collage-space coordinates
fixPosition : (Int, Int) -> (Float, Float) -> (Float, Float)
fixPosition (w, h) (x, y) =
  (x - toFloat w / 2, -y + toFloat h / 2)
