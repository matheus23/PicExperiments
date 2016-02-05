module App where

import Graphics.Element exposing (Element)
import Reactive exposing (Reactive, Event(..), TouchType(..), Navigation(..))
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

    makeElement size (_, { visual }, _) =
      Pic.toElement size visual
  in
    Signal.map2 makeElement Window.dimensions
    <| Signal.foldp updateReactive (init, view init, Nowhere) (eventsSignal Window.dimensions)

eventsSignal : Signal (Int, Int) -> Signal Event
eventsSignal collageSize =
  let
    makeFloatVec (intX, intY) = (toFloat intX, toFloat intY)
    fixedTouchPos = Signal.map2 fixPosition collageSize ExtraSignals.fingerPosition
    fixedMousePos = Signal.map2 fixPosition collageSize (Signal.map makeFloatVec Mouse.position)
   in Signal.mergeMany
        [ Signal.map (TouchEvent FingerDown) (Signal.sampleOn ExtraSignals.fingerPressed fixedTouchPos)
        , Signal.map (TouchEvent FingerUp) (Signal.sampleOn ExtraSignals.fingerReleased fixedTouchPos)
        , Signal.map (TouchEvent FingerMove) fixedTouchPos
        , Signal.map (TouchEvent FingerMove) fixedMousePos
        ]

-- turns event-space coordinates to collage-space coordinates
fixPosition : (Int, Int) -> (Float, Float) -> (Float, Float)
fixPosition (w, h) (x, y) =
  (x - toFloat w / 2, -y + toFloat h / 2)
