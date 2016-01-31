module Main where

import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Text
import Window
import Color
import Mouse
import Pic exposing (..)


type alias Reactive graphic action =
  { visual : graphic
  , reaction : (Float, Float) -> List action
  }


nonReactive : Pic -> Reactive Pic action
nonReactive picture =
  { visual = picture
  , reaction = \x -> []
  }


withMousePos : Pic -> Reactive Pic (Float, Float)
withMousePos picture =
  { visual = picture
  , reaction = \event -> [event]
  }


someCircle : Pic
someCircle =
  filled Color.red (circle 10)


main : Signal Element
main =
  let
    myPic =
      atop (text (Text.bold <| Text.fromString "whatever")) someCircle

    update pos _ = pos

    view pos =
      withMousePos (move pos myPic)
  in
    runReactive Mouse.position (0, 0) update view


runReactive
  : Signal (Int, Int)
  -> model
  -> (action -> model -> model)
  -> (model -> Reactive Pic action)
  -> Signal Element
runReactive events model update view =
  let
    updateState newModel =
      (newModel, view newModel)

    updateReactive event (model, reactive) =
      updateState <| List.foldl update model (reactive.reaction event)

    makeElement size (_, { visual }) =
      toElement size visual
  in
    events
      |> Signal.map2 fixMouse Window.dimensions
      |> Signal.foldp updateReactive (model, view model)
      |> Signal.map2 makeElement Window.dimensions


fixMouse : (Int, Int) -> (Int, Int) -> (Float, Float)
fixMouse (w, h) (x, y) =
  (toFloat x - toFloat w / 2, toFloat -y + toFloat h / 2)
