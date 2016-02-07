module Main where

import Graphics.Element exposing (Element)
import Signal exposing (Signal)
import Text
import Color
import Pic exposing (Pic, pic)
import Reactive exposing (Reactive, Event(..), reactive)
import App exposing (App)
import Button exposing (Action(..))
import PicLike exposing (..)
import Dim exposing (..)

type Action = Add

someCircle : Pic
someCircle =
  Pic.filled Color.red (Pic.circle 10)

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
      nextTo reactive Down
        (Button.onPress Add (countButton.view buttonState))
        (scale reactive 10 <| Reactive.static <| Pic.text <| Text.fromString <| toString value)

  in App.run { init = (0, countButton.init), update = update, view = view }
