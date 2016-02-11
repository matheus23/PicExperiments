module Reactive where

import Pic exposing (Pic, pic)
import Dim exposing (..)
import PicLike exposing (..)

type alias Pos = (Float, Float)

type Event
  = TouchEvent TouchType Pos

type TouchType
  = FingerDown
  | FingerUp
  | FingerMove

type alias Reactive message =
  { visual : Pic
  , reaction : Event -> Maybe message
  }

reactive : PicLike (Reactive message)
reactive =
  { move_ = moveReactive
  , scale_ = scaleReactive
  , atop_ = atopReactive
  , empty_ = static (empty pic)
  , dimensions_ = .visual >> .picSize
  }

-- almost like liftReactive0
static : Pic -> Reactive message
static picture =
  { visual = picture
  , reaction = \_ -> Nothing
  }

makeStatic : Reactive a -> Reactive b
makeStatic reactive = static reactive.visual

liftReactive : (Pic -> Pic) -> (Pos -> Pos) -> Reactive message -> Reactive message
liftReactive mapPic mapPosition reactive =
  let
    mapEvent (TouchEvent evType pos) =
      TouchEvent evType (mapPosition pos)
  in
    { visual = mapPic reactive.visual
    , reaction = mapEvent >> reactive.reaction
    }

forwardMessage : (messageA -> Maybe messageB) -> Reactive messageA -> Reactive messageB
forwardMessage mapping reactive =
  { visual = reactive.visual
  , reaction = \ev -> reactive.reaction ev `Maybe.andThen` mapping
  }

alwaysForwardMessage : (messageA -> messageB) -> Reactive messageA -> Reactive messageB
alwaysForwardMessage mapping reactive = forwardMessage (\msg -> Just (mapping msg)) reactive

atopReactive : Reactive message -> Reactive message -> Reactive message
atopReactive reactiveAbove reactiveBelow =
  { visual = atop pic reactiveAbove.visual reactiveBelow.visual
  , reaction = delegateEvent reactiveAbove reactiveBelow
  }

delegateEvent : Reactive message -> Reactive message -> Event -> Maybe message
delegateEvent reactiveAbove reactiveBelow (TouchEvent evType pos) =
  let
    dimAbove = reactiveAbove.visual.picSize
    dimBelow = reactiveBelow.visual.picSize
   in
      if isInside pos dimAbove then
        reactiveAbove.reaction (TouchEvent evType pos)
      else if isInside pos dimBelow then
        reactiveBelow.reaction (TouchEvent evType pos)
      else if isInside pos (Pic.atopDims dimAbove dimBelow) then
        Nothing
      else
        Nothing


isInside : Pos -> Dim -> Bool
isInside (x, y) dims =
  let between lower value upper = lower <= value && value <= upper
   in between dims.toLeft x dims.toRight && between dims.toBottom y dims.toTop

moveReactive : Pos -> Reactive message -> Reactive message
moveReactive offset = liftReactive (move pic offset) (moveEvent offset)

moveEvent : Pos -> Pos -> Pos
moveEvent (offx, offy) (x, y) = (x-offx, y-offy)

scaleReactive : Float -> Reactive message -> Reactive message
scaleReactive factor = liftReactive (scale pic factor) (scalePick factor)

scalePick : Float -> Pos -> Pos
scalePick factor (x, y) = (x / factor, y / factor)

padded : Float -> Reactive message -> Reactive message
padded padding = liftReactive (Pic.padded padding) identity

onEvent : (Event -> Maybe message) -> Reactive message -> Reactive message
onEvent getMessage reactive =
  let
    react event =
      Maybe.oneOf [ reactive.reaction event, getMessage event ]
   in { visual = reactive.visual
      , reaction = react
      }

onFingerEvent : TouchType -> (Pos -> Maybe message) -> Reactive message -> Reactive message
onFingerEvent filterType getMessage reactive =
  let
    react (TouchEvent evType pos) =
      if evType == filterType then
        getMessage pos
      else
        Nothing

   in onEvent react reactive

onFingerDown : (Pos -> Maybe message) -> Reactive message -> Reactive message
onFingerDown = onFingerEvent FingerDown

onFingerUp : (Pos -> Maybe message) -> Reactive message -> Reactive message
onFingerUp = onFingerEvent FingerUp

onFingerMove : (Pos -> Maybe message) -> Reactive message -> Reactive message
onFingerMove = onFingerEvent FingerMove
