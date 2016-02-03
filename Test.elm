module Main where

import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Text
import Color
import Pic exposing (..)
import Reactive exposing (Reactive, Event(..))
import App exposing (App)
import Button exposing (..)

type Action = NoAction | Add

someCircle : Pic
someCircle =
  filled Color.red (circle 10)

main : Signal Element
main =
  let
    countButton = textButton "+1"

    update (action, buttonAction) (value, buttonState) = (updateValue action value, countButton.update buttonAction buttonState)

    updateValue action value =
      case action of
        NoAction -> value
        Add -> value + 1

    makeAdd buttonAction =
      case buttonAction of
        Press -> Just (NoAction, Press)
        Release -> Just (Add, Release)

    view (value, buttonState) =
      Reactive.nextTo Down
        (Reactive.forwardMessage makeAdd (countButton.view buttonState))
        (Reactive.scale 10 <| Reactive.static <| text <| Text.fromString <| toString value)

  in App.run { init = (0, countButton.init), update = update, view = view }
