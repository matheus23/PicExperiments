module Dragh where

import Graphics.Element exposing (Element)
import Pic exposing (Pic, pic)
import Dim exposing (..)
import PicLike exposing (..)
import Text exposing (Text)
import App exposing (App)
import Reactive exposing (Reactive, Event(..), reactive)
import Color exposing (Color)


type Shit
  = Hole
  | GrabMe Color
  | Besides Shit Shit


type DragState
  = NothingDragged
  | Dragged Shit Shit (Float, Float) (Float, Float)


holePic : Float -> Pic
holePic radius =
  let
    greyRect =
      Pic.outlined (Pic.solid Color.darkGrey) <| Pic.rectFromDim <| Pic.squareDim radius

    questionMark =
      (scale pic 3 <| Pic.text <| Text.color Color.darkGrey <| Text.fromString "?")
  in
    atop pic questionMark greyRect


grabMe : Color -> Pic
grabMe color =
  let
    eye =
      Pic.filled Color.black (Pic.circle 3)

    leftEye =
      move pic (-10, 10) eye

    rightEye =
      move pic (10, 10) eye

    mouth =
      Pic.filled Color.white (Pic.rectFromDim { toLeft = -15, toRight = 15, toTop = -5, toBottom = -15 })

    blockHead =
      Pic.filled color (Pic.rectFromDim (Pic.squareDim 20))

    ear =
      Pic.filled color (Pic.circle 8)

    ears =
      atop pic (move pic (-15, 20) ear) (move pic (15, 20) ear)

    body =
      atopAll pic
        [ leftEye
        , rightEye
        , mouth
        , blockHead
        ]
  in
    Pic.withDim body.picSize
      (atop pic body ears)


dropReaction : DragState -> (Shit -> Shit) -> Maybe (Shit, DragState)
dropReaction dragState pack =
  case dragState of
    NothingDragged ->
      Nothing

    Dragged shit undoDrag fingerPos grabPos ->
      Just (pack shit, NothingDragged)


dragReaction : DragState -> (Shit -> Shit) -> Shit -> Shit -> (Float, Float) -> Maybe (Shit, DragState)
dragReaction dragState pack whatToDrag replacement grabPos =
  case dragState of
    NothingDragged ->
      Just (pack replacement, Dragged whatToDrag (pack whatToDrag) (0, 0) grabPos)

    Dragged _ revertChanges fingerPos grabPos ->
      Nothing


viewHole : (Shit -> Shit) -> DragState -> Reactive (Shit, DragState)
viewHole pack dragState =
  let
    holeStatic =
      Reactive.static (holePic 20)
  in
    Reactive.onFingerUp (always <| dropReaction dragState pack) holeStatic


viewGrabMe : Color -> (Shit -> Shit) -> DragState -> Reactive (Shit, DragState)
viewGrabMe color pack dragState =
  let
    grabMeStatic =
      Reactive.static (grabMe color)
  in
    Reactive.onFingerDown (dragReaction dragState pack (GrabMe color) Hole)
      grabMeStatic


viewBesides : (Shit -> Shit) -> DragState -> Shit -> Shit -> Reactive (Shit, DragState)
viewBesides pack dragState leftShit rightShit =
  let
    updateLeft newLeft = Besides newLeft rightShit
    updateRight newRight = Besides leftShit newRight
  in
    centered reactive
      (nextTo reactive Right
        (Reactive.padded 4 (view (pack << updateLeft) (leftShit, dragState)))
        (Reactive.padded 4 (view (pack << updateRight) (rightShit, dragState))))


view : (Shit -> Shit) -> (Shit, DragState) -> Reactive (Shit, DragState)
view pack (shit, dragState) =
  case shit of
    Hole ->
      viewHole pack dragState

    GrabMe color ->
      viewGrabMe color pack dragState

    Besides leftShit rightShit ->
      viewBesides pack dragState leftShit rightShit


viewWhole : (Shit, DragState) -> Reactive (Shit, DragState)
viewWhole (shit, dragState) =
  case dragState of
    NothingDragged ->
      view identity (shit, dragState)

    Dragged currentlyDragging reset fingerPos grabPos ->
      let
        vecSub (a, b) (u, v) = (a - u, b - v)
        reactMove pos = Just (shit, Dragged currentlyDragging reset pos grabPos)
        showDraggedAtop picture =
          atop pic
            (move pic (vecSub fingerPos grabPos) (view identity (currentlyDragging, NothingDragged)).visual)
            picture
        failDrag _ = Just (reset, NothingDragged)
      in
        Reactive.onFingerUp failDrag
        <| Reactive.onFingerMove reactMove
            (Reactive.liftReactive showDraggedAtop identity
              (view identity (shit, dragState)))


main : Signal Element
main = App.run
  { init = (Besides (Besides (GrabMe Color.red) Hole) (GrabMe Color.green), NothingDragged)
  , update = always
  , view = viewWhole
  }
