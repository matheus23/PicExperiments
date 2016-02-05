module Main where

import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Text
import Color
import Pic exposing (..)
import Reactive exposing (Reactive, Event(..))
import App exposing (App)
import Button exposing (Action(..))

type Action = Add

someCircle : Pic
someCircle =
  filled Color.red (circle 10)

main : Signal Element
main =
  let
    countButton = Button.textButton "+1"

    update (buttonAction, mayAction) (value, buttonState) =
      (updateMaybe updateValue mayAction value, countButton.update buttonAction buttonState)

    updateMaybe update mayAction value =
      Maybe.withDefault value (Maybe.map (\action -> update action value) mayAction)

    updateValue Add value = value + 1

    view (value, buttonState) =
      Reactive.nextTo Down
        (Button.onPress Add (countButton.view buttonState))
        (Reactive.scale 10 <| Reactive.static <| text <| Text.fromString <| toString value)

  in App.run { init = (0, countButton.init), update = update, view = view }
