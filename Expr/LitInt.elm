module Expr.LitInt where

import Graphics.Element exposing (Element)
import Pic exposing (Pic, Direction(..))
import Text exposing (Text)
import App exposing (App)
import Reactive exposing (Reactive, Event(..))
import Color

type alias Model =
  { intValue : Int
  , state : DragginState
  }

type DragginState
  = NotDraggin
  | Draggin Int Float

type Action
  = StartDraggin
  | ChangeDraggin Float
  | StopDraggin

fromInt : Int -> Model
fromInt int =
  { intValue = int
  , state = NotDraggin
  }

view : Model -> Reactive Action
view { intValue, state } =
  case state of
    NotDraggin ->
      Reactive.onFingerDown (always <| Just StartDraggin) <| Reactive.static <| defaultText (toString intValue)
    Draggin startValue delta ->
      draggingView startValue delta

draggingView : Int -> Float -> Reactive Action
draggingView intValue delta =
  let changeValue (_, y) = ChangeDraggin -y
   in
    Reactive.static (showWheel intValue delta)
    |> Reactive.onFingerUp (always <| Just StopDraggin)
    |> Reactive.onFingerMove (Just << changeValue)

update : Action -> Model -> Model
update action { intValue, state } =
  case state of
    NotDraggin ->
      case action of
        StartDraggin ->
          { intValue = intValue, state = Draggin intValue 0 }
        StopDraggin ->
          { intValue = intValue, state = NotDraggin }
        ChangeDraggin delta ->
          { intValue = intValue, state = NotDraggin }
    Draggin startValue delta ->
      case action of
        StartDraggin ->
          { intValue = intValue, state = Draggin startValue delta }
        StopDraggin ->
          { intValue = startValue + valueChange delta, state = NotDraggin }
        ChangeDraggin newDelta ->
          { intValue = startValue + valueChange delta, state = Draggin startValue newDelta }

wheelElemHeight : Float
wheelElemHeight = 40

wheelSize : Int
wheelSize = 5

valueChange : Float -> Int
valueChange delta = round (delta / wheelElemHeight)

defaultText : String -> Pic
defaultText = Pic.scale 3 << Pic.text << Text.fromString

defaultTextGreen : String -> Pic
defaultTextGreen = Pic.scale 3 << Pic.text << Text.color Color.green << Text.fromString

appendWithRefDim : Direction -> Pic -> List Pic -> Pic
appendWithRefDim inDirection reference pics =
  Pic.withDim reference.picSize (Pic.appendTo inDirection reference pics)

showWheel : Int -> Float -> Pic
showWheel intValue dragDistance =
  let
    actualValue = intValue + valueChange dragDistance
    offset = toFloat (valueChange dragDistance) * wheelElemHeight - dragDistance
    offsetFromDiff diff = (toFloat diff) * wheelElemHeight + offset
    alphaFromDiff diff = Pic.alpha (1 - (abs <| offsetFromDiff diff) / (toFloat wheelSize * wheelElemHeight))

    valueText = defaultTextGreen (toString actualValue)
    valuesDiffBefore = List.map negate [1 .. wheelSize]
    valuesDiffAfter = [1 .. wheelSize]
    createValue diff = Pic.move (0, offsetFromDiff diff) (alphaFromDiff diff (defaultText (toString (actualValue + diff))))
    valuesBefore = List.map createValue valuesDiffBefore
    valuesAfter = List.map createValue valuesDiffAfter
    wheel = Pic.atopAll (valuesBefore ++ [Pic.move (0, offset) valueText] ++ valuesAfter)
   in
    Pic.withDim valueText.picSize wheel
