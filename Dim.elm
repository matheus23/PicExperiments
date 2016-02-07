module Dim where

type alias Dim =
  { toLeft : Float
  , toTop : Float
  , toRight : Float
  , toBottom : Float
  }

type Direction = Left | Right | Up | Down

opposite : Direction -> Direction
opposite dir =
  case dir of
    Left -> Right
    Right -> Left
    Up -> Down
    Down -> Up

toBorderInDir : Direction -> Dim -> (Float, Float)
toBorderInDir direction box =
  case direction of
    Left -> (box.toLeft, 0)
    Right -> (box.toRight, 0)
    Up -> (0, box.toTop)
    Down -> (0, box.toBottom)

-- "1D Vector from left side to right side"
width : Dim -> Float
width dim = dim.toRight - dim.toLeft

-- "1D Vector from bottom to top"
height : Dim -> Float
height dim = dim.toTop - dim.toBottom
