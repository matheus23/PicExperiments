module App where

import Graphics.Element exposing (Element)
import Reactive exposing (Reactive, Event(..), TouchType(..))
import Pic exposing (Pic)
import ExtraSignals
import Window

type alias App model message =
  { init : model
  , update : message -> model -> model
  , view : model -> Reactive message
  }

-- Running Apps

run : App model message -> Signal Element
run { init, update, view } =
  let
    -- updateState : model -> (model, Reactive message)
    updateState newModel =
      (newModel, view newModel)

    -- updateReactive : Event -> (model, Reactive message) -> (model, Reactive message)
    updateReactive event (model, reactive) =
      case reactive.reaction event of
        Just message ->
          updateState (update message model)

        Nothing ->
          (model, reactive)

    -- makeElement : (Int, Int) -> (model, Reactive message) -> Element
    makeElement size (_, { visual }) =
      Pic.toElement size visual
  in
    Signal.map2 makeElement Window.dimensions
    <| Signal.foldp updateReactive (init, view init) (eventsSignal Window.dimensions)

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
