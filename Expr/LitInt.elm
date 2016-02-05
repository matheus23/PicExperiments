module Expr.LitInt where

import Pic exposing (Pic, Direction(..))
import Text exposing (Text)
import Reactive exposing (Reactive, Event(..))
import Color

type alias Model =
  { intValue : Int
  , state : DraggingState
  }

type DraggingState
  = NotDragging
  | Dragging Int Float

fromInt : Int -> Model
fromInt int =
  { intValue = int
  , state = NotDragging
  }

view : Model -> Reactive Model
view { intValue, state } =
  case state of
    NotDragging ->
      Reactive.onFingerDown (always <| Just { intValue = intValue, state = Dragging intValue 0 })
      <| Reactive.static
      <| defaultText (toString intValue)
    Dragging startValue delta ->
      draggingView startValue delta

draggingView : Int -> Float -> Reactive Model
draggingView startValue delta =
  let changeValue (_, y) = { intValue = startValue + valueChange -y, state = Dragging startValue -y }
   in
    Reactive.static (showWheel startValue delta)
    |> Reactive.onFingerUp (always <| Just { intValue = startValue + valueChange delta, state = NotDragging })
    |> Reactive.onFingerMove (Just << changeValue)

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
