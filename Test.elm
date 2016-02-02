module Main where

import Graphics.Collage exposing (solid)
import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Text
import Color
import Pic exposing (..)
import Reactive exposing (Reactive, Event(..))
import RunReactive

type Action = Add

someCircle : Pic
someCircle =
  filled Color.red (circle 10)

renderBoundingBox : Pic -> Pic
renderBoundingBox picture =
  (outlined (solid Color.red) (rectFromDim picture.picSize)) `atop` picture

main : Signal Element
main =
  let
    update Add value = value + 1

    view value =
      Reactive.scale 10 <| Reactive.nextTo Down
        (Reactive.onClick (always (Just Add)) (Reactive.static someCircle))
        (Reactive.static <| text <| Text.fromString <| toString value)

  in RunReactive.runReactive 0 update view
