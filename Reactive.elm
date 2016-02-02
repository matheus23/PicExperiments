module Reactive where

import Pic exposing (Pic, Direction)

type Event
  = MouseClick (Float, Float)

type alias Reactive action =
  { visual : Pic
  , reaction : Event -> Maybe action
  }

-- almost like liftReactive0
static : Pic -> Reactive action
static picture =
  { visual = picture
  , reaction = \x -> Nothing
  }

liftReactive : (Pic -> Pic) -> (Event -> Event) -> Reactive action -> Reactive action
liftReactive mapPic mapEvent reactive =
  { visual = mapPic reactive.visual
  , reaction = reactive.reaction << mapEvent
  }

nextTo
  : Direction
  -> Reactive action
  -> Reactive action
  -> Reactive action
nextTo inDirection reference reactive =
  let offset = Pic.offsetNextTo inDirection reference.visual reactive.visual
   in atop (move offset reactive) reference

atop
  : Reactive action
  -> Reactive action
  -> Reactive action
atop reactiveAbove reactiveBelow =
  { visual = Pic.atop reactiveAbove.visual reactiveBelow.visual
  , reaction = delegateEvent reactiveAbove reactiveBelow
  }

delegateEvent
  : Reactive action
  -> Reactive action
  -> (Event -> Maybe action)
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

move : (Float, Float) -> Reactive action -> Reactive action
move offset = liftReactive (Pic.move offset) (moveEvent offset)

moveEvent : (Float, Float) -> Event -> Event
moveEvent (offx, offy) (MouseClick (x, y)) = MouseClick (x+offx, y+offy)

scale : Float -> Reactive action -> Reactive action
scale factor = liftReactive (Pic.scale factor) (scaleEvent factor)

scaleEvent : Float -> Event -> Event
scaleEvent factor (MouseClick (x, y)) = (MouseClick (x / factor, y / factor))

padded : Float -> Reactive action -> Reactive action
padded padding = liftReactive (Pic.padded padding) identity

onClick : ((Float, Float) -> Maybe action) -> Reactive action -> Reactive action
onClick action reactive =
  let react (MouseClick pos) =
        if isInside pos reactive.visual.picSize
          then Maybe.oneOf [ reactive.reaction (MouseClick pos), action pos ]
          else Nothing
   in { visual = reactive.visual
      , reaction = react
      }
