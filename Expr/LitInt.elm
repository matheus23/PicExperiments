module Expr.LitInt where

import Graphics.Element exposing (Element)
import Pic exposing (Pic, Direction(..))
import Text exposing (Text)
import App exposing (App)
import Reactive exposing (Reactive, Event(..))
import Color

type alias Model =
  { intValue : Int
  , state : ScrubbingState
  }

type ScrubbingState
  = NotScrubbing
  | Scrubbing Int

type Action
  = StartScrubbing
  | ChangeScrubbing Int
  | StopScrubbing

fromInt : Int -> Model
fromInt int =
  { intValue = int
  , state = NotScrubbing
  }

view : Model -> Reactive Action
view { intValue, state } =
  case state of
    NotScrubbing ->
      Reactive.onFingerDown (always <| Just StartScrubbing) <| Reactive.static <| defaultText (toString intValue)
    Scrubbing delta ->
      scrubbingView intValue delta

scrubbingView : Int -> Int -> Reactive Action
scrubbingView intValue delta =
  let
    value = intValue + delta
    textPic = defaultTextGreen (toString value)
    changeValue (_, y) = ChangeScrubbing (round (y / Pic.height textPic))
   in
    Reactive.static textPic
    |> Reactive.onFingerUp (always <| Just StopScrubbing)
    |> Reactive.onFingerMove (Just << changeValue)

update : Action -> Model -> Model
update action { intValue, state } =
  case state of
    NotScrubbing ->
      case action of
        StartScrubbing ->
          { intValue = intValue, state = Scrubbing 0 }
        StopScrubbing ->
          { intValue = intValue, state = NotScrubbing }
        ChangeScrubbing delta ->
          { intValue = intValue, state = NotScrubbing }
    Scrubbing delta ->
      case action of
        StartScrubbing ->
          { intValue = intValue, state = Scrubbing delta }
        StopScrubbing ->
          { intValue = intValue + delta, state = NotScrubbing }
        ChangeScrubbing newDelta ->
          { intValue = intValue, state = Scrubbing newDelta }

defaultText : String -> Pic
defaultText = Pic.scale 3 << Pic.text << Text.fromString

defaultTextGreen : String -> Pic
defaultTextGreen = Pic.scale 3 << Pic.text << Text.color Color.green << Text.fromString

appendWithRefDim : Direction -> Pic -> List Pic -> Pic
appendWithRefDim inDirection reference pics =
  Pic.withDim reference.picSize (Pic.appendTo inDirection reference pics)
