module Reactive where

import Pic exposing (Pic, Direction)

type alias Pos = (Float, Float)

type Event
  = TouchEvent TouchType Pos

type TouchType
  = FingerDown
  | FingerUp
  | FingerMove

-- Shows where to find reactive:
type Navigation
  = Here
  | Nowhere
  | Above Navigation
  | Below Navigation

type alias Reactive message =
  { visual : Pic
  , pick : Pos -> Navigation
  , reaction : Event -> Navigation -> Maybe message
  }

-- almost like liftReactive0
static : Pic -> Reactive message
static picture =
  { visual = picture
  , pick = \pos -> if isInside pos (picture.picSize) then Here else Nowhere
  , reaction = \_ _ -> Nothing
  }

liftReactive : (Pic -> Pic) -> (Pos -> Pos) -> Reactive message -> Reactive message
liftReactive mapPic mapPosition reactive =
  { visual = mapPic reactive.visual
  , pick = reactive.pick << mapPosition
  , reaction = reactive.reaction
  }

forwardMessage : (messageA -> Maybe messageB) -> Reactive messageA -> Reactive messageB
forwardMessage mapping reactive =
  { visual = reactive.visual
  , pick = reactive.pick
  , reaction = \ev nav -> reactive.reaction ev nav `Maybe.andThen` mapping
  }

alwaysForwardMessage : (messageA -> messageB) -> Reactive messageA -> Reactive messageB
alwaysForwardMessage mapping reactive = forwardMessage (\msg -> Just (mapping msg)) reactive

nextTo
  : Direction
  -> Reactive message
  -> Reactive message
  -> Reactive message
nextTo inDirection reference reactive =
  let offset = Pic.offsetNextTo inDirection reference.visual reactive.visual
   in atop (move offset reactive) reference

atop
  : Reactive message
  -> Reactive message
  -> Reactive message
atop reactiveAbove reactiveBelow =
  { visual = Pic.atop reactiveAbove.visual reactiveBelow.visual
  , pick = findOutWhich reactiveAbove reactiveBelow
  , reaction = delegateEvent reactiveAbove reactiveBelow
  }

findOutWhich : Reactive message -> Reactive message -> Pos -> Navigation
findOutWhich reactiveAbove reactiveBelow pos =
  let
    dimAbove = reactiveAbove.visual.picSize
    dimBelow = reactiveBelow.visual.picSize
   in
      if isInside pos dimAbove then
        Above (reactiveAbove.pick pos)
      else if isInside pos dimBelow then
        Below (reactiveBelow.pick pos)
      else if isInside pos (Pic.atopDims dimAbove dimBelow) then
        Here
      else
        Nowhere

delegateEvent : Reactive message -> Reactive message -> Event -> Navigation -> Maybe message
delegateEvent reactiveAbove reactiveBelow event navigation =
  case navigation of
    Here -> Nothing
    Nowhere -> Nothing
    Above nav -> reactiveAbove.reaction event nav
    Below nav -> reactiveBelow.reaction event nav

isInside : Pos -> Pic.Dim -> Bool
isInside (x, y) dims =
  let between lower value upper = lower <= value && value <= upper
   in between dims.toLeft x dims.toRight && between dims.toBottom y dims.toTop

move : Pos -> Reactive message -> Reactive message
move offset = liftReactive (Pic.move offset) (movePick offset)

movePick : Pos -> Pos -> Pos
movePick (offx, offy) (x, y) = (x-offx, y-offy)

scale : Float -> Reactive message -> Reactive message
scale factor = liftReactive (Pic.scale factor) (scalePick factor)

scalePick : Float -> Pos -> Pos
scalePick factor (x, y) = (x / factor, y / factor)

padded : Float -> Reactive message -> Reactive message
padded padding = liftReactive (Pic.padded padding) identity

onEvent : (Event -> Maybe message) -> Reactive message -> Reactive message
onEvent getMessage reactive =
  let
    react event navigation =
      case navigation of
        Here -> Maybe.oneOf [ getMessage event, reactive.reaction event navigation ]
        otherwise -> reactive.reaction event navigation
   in { visual = reactive.visual
      , pick = reactive.pick
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
