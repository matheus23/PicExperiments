module Reactive where

import Pic exposing (Pic, Direction)

type Event
  = TouchEvent TouchType (Float, Float)

type TouchType
  = FingerDown
  | FingerUp
  | FingerMove

type alias Reactive message =
  { visual : Pic
  , reaction : Event -> Maybe message
  }

-- almost like liftReactive0
static : Pic -> Reactive message
static picture =
  { visual = picture
  , reaction = \x -> Nothing
  }

liftReactive : (Pic -> Pic) -> (Event -> Event) -> Reactive message -> Reactive message
liftReactive mapPic mapEvent reactive =
  { visual = mapPic reactive.visual
  , reaction = reactive.reaction << mapEvent
  }

forwardMessage : (messageA -> Maybe messageB) -> Reactive messageA -> Reactive messageB
forwardMessage mapping reactive =
  let mappedReaction event =
        case reactive.reaction event of
          Just message -> mapping message
          Nothing -> Nothing

   in { visual = reactive.visual
      , reaction = mappedReaction
      }

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
  , reaction = delegateEvent reactiveAbove reactiveBelow
  }

delegateEvent
  : Reactive message
  -> Reactive message
  -> (Event -> Maybe message)
delegateEvent reactiveAbove reactiveBelow (TouchEvent evType pos) =
  let insideAbove = isInside pos (reactiveAbove.visual.picSize)
      insideBelow = isInside pos (reactiveBelow.visual.picSize)
      reactionIfInside itsInside reaction = if itsInside then reaction (TouchEvent evType pos) else Nothing
   in Maybe.oneOf
        [ reactionIfInside insideAbove reactiveAbove.reaction
        , reactionIfInside insideBelow reactiveBelow.reaction
        ]

isInside : (Float, Float) -> Pic.Dim -> Bool
isInside (x, y) dims =
  let between lower value upper = lower <= value && value <= upper
   in between dims.toLeft x dims.toRight && between dims.toBottom y dims.toTop

move : (Float, Float) -> Reactive message -> Reactive message
move offset = liftReactive (Pic.move offset) (moveEvent offset)

moveEvent : (Float, Float) -> Event -> Event
moveEvent (offx, offy) (TouchEvent evType (x, y)) = TouchEvent evType (x+offx, y+offy)

scale : Float -> Reactive message -> Reactive message
scale factor = liftReactive (Pic.scale factor) (scaleEvent factor)

scaleEvent : Float -> Event -> Event
scaleEvent factor (TouchEvent evType (x, y)) = (TouchEvent evType (x / factor, y / factor))

padded : Float -> Reactive message -> Reactive message
padded padding = liftReactive (Pic.padded padding) identity

onEvent : (Event -> Maybe message) -> Reactive message -> Reactive message
onEvent getMessage reactive =
  let react (TouchEvent evType pos) =
        if isInside pos reactive.visual.picSize
          then Maybe.oneOf [ reactive.reaction (TouchEvent evType pos), getMessage (TouchEvent evType pos) ]
          else Nothing
   in { visual = reactive.visual
      , reaction = react
      }

onFingerEvent : TouchType -> ((Float, Float) -> Maybe message) -> Reactive message -> Reactive message
onFingerEvent filterType message reactive =
  let
    react (TouchEvent evType pos) =
      if evType == filterType && isInside pos reactive.visual.picSize then
        Maybe.oneOf [ reactive.reaction (TouchEvent evType pos), message pos ]
      else
        Nothing
   in
    { visual = reactive.visual
    , reaction = react
    }

onFingerDown : ((Float, Float) -> Maybe message) -> Reactive message -> Reactive message
onFingerDown = onFingerEvent FingerDown
