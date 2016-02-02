module Reactive where

import Pic exposing (Pic, Direction)

type Event
  = MouseClick (Float, Float)

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
delegateEvent reactiveAbove reactiveBelow (MouseClick pos) =
  let insideAbove = isInside pos (reactiveAbove.visual.picSize)
      insideBelow = isInside pos (reactiveBelow.visual.picSize)
      reactionIfInside itsInside reaction = if itsInside then reaction (MouseClick pos) else Nothing
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
moveEvent (offx, offy) (MouseClick (x, y)) = MouseClick (x+offx, y+offy)

scale : Float -> Reactive message -> Reactive message
scale factor = liftReactive (Pic.scale factor) (scaleEvent factor)

scaleEvent : Float -> Event -> Event
scaleEvent factor (MouseClick (x, y)) = (MouseClick (x / factor, y / factor))

padded : Float -> Reactive message -> Reactive message
padded padding = liftReactive (Pic.padded padding) identity

onClick : ((Float, Float) -> Maybe message) -> Reactive message -> Reactive message
onClick message reactive =
  let react (MouseClick pos) =
        if isInside pos reactive.visual.picSize
          then Maybe.oneOf [ reactive.reaction (MouseClick pos), message pos ]
          else Nothing
   in { visual = reactive.visual
      , reaction = react
      }
