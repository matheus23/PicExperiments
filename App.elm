module App where

import Graphics.Element exposing (Element)
import Reactive exposing (Reactive, Event(..), TouchType(..))
import Pic exposing (Pic)
import ExtraSignals
import Window
import Mouse

type alias App model message =
  { init : model
  , update : message -> model -> model
  , view : model -> Reactive message
  }

-- Running Apps
run : App model message -> Signal Element
run { init, update, view } =
  let
    updateState newModel =
      (newModel, view newModel)

    updateReactive event (model, reactive) =
      case reactive.reaction event of
        Just message ->
          updateState (update message model)

        Nothing ->
          (model, reactive)

    makeElement size (_, { visual }) =
      Pic.toElement size visual
  in
    Signal.map2 makeElement Window.dimensions
    <| Signal.foldp updateReactive (init, view init) (eventsSignal Window.dimensions)

eventsSignal : Signal (Int, Int) -> Signal Event
eventsSignal collageSize =
  let
    makeFloatVec (intX, intY) = (toFloat intX, toFloat intY)
    fixedTouchPos = Signal.map2 fixPosition collageSize ExtraSignals.fingerPosition
    fixedMousePos = Signal.map2 fixPosition collageSize (Signal.map makeFloatVec Mouse.position)
    mouseDown = Signal.filter (\isDown -> isDown == True) True Mouse.isDown
    mouseUp = Signal.filter (\isDown -> isDown == False) False Mouse.isDown
    mousePress = Signal.sampleOn mouseDown fixedMousePos
    mouseRelease = Signal.sampleOn mouseUp fixedMousePos
   in Signal.mergeMany
        [ {-Signal.map (TouchEvent FingerDown) mousePress
        , Signal.map (TouchEvent FingerUp) mouseRelease
        , Signal.map (TouchEvent FingerMove) fixedMousePos
        ,-} Signal.map (TouchEvent FingerDown) (Signal.sampleOn ExtraSignals.fingerPressed fixedTouchPos)
        , Signal.map (TouchEvent FingerUp) (Signal.sampleOn ExtraSignals.fingerReleased fixedTouchPos)
        , Signal.map (TouchEvent FingerMove) fixedTouchPos
        ]

-- turns event-space coordinates to collage-space coordinates
fixPosition : (Int, Int) -> (Float, Float) -> (Float, Float)
fixPosition (w, h) (x, y) =
  (x - toFloat w / 2, -y + toFloat h / 2)
