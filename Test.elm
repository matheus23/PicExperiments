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

-- hides the button App
-- at least... it should do that
-- but it's actually impossible to implement
embed
  : App Button.State Button.Action
  -> model
  -> (message -> model -> model)
  -> (Reactive Button.Action -> model -> Reactive (Button.Action, message))
  -> App model message
embed button outerInit outerUpdate strangeView =
  let
    update (outerAction, buttonAction) (outerModel, buttonModel) =
      (button.update buttonAction buttonModel, updateMaybe outerUpdate outerAction outerModel)

    updateMaybe update mayMessage model =
      Maybe.withDefault model (Maybe.map (\message -> update message model) mayMessage)

    view (buttonModel, outerModel) = strangeView (button.view buttonModel) outerModel

    hideInner appWithEmbedded =
      { init = snd appWithEmbedded.init
      , update = } -- what to do now? we can't update the button without knowing it's previous state!
   in doSth
    { init = (button.init, outerInit)
    , update = update
    , view = view
    }

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
