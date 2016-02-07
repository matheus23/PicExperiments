module Pic where

import Graphics.Element as E
import Graphics.Collage as C
import Text as T
import Color exposing (Color)
import Dim exposing (..)
import PicLike exposing (..)

type alias Pic =
  { asForm : C.Form
  , picSize : Dim
  }

type alias Shape =
  { elmShape : C.Shape
  , shapeSize : Dim
  }

-- ABSTRACT

pic : PicLike Pic
pic =
  { move_ = movePic
  , scale_ = scalePic
  , atop_ = atopPic
  , empty_ = { asForm = C.group [], picSize = Dim 0 0 0 0 }
  , dimensions_ = .picSize
  }

atopPic : Pic -> Pic -> Pic
atopPic picAtop picBelow =
  { asForm = C.group [ picBelow.asForm, picAtop.asForm ]
  , picSize = atopDims picAtop.picSize picBelow.picSize
  }

atopDims : Dim -> Dim -> Dim
atopDims atopDim belowDim =
  { toLeft = min atopDim.toLeft belowDim.toLeft
  , toTop = max atopDim.toTop belowDim.toTop
  , toRight = max atopDim.toRight belowDim.toRight
  , toBottom = min atopDim.toBottom belowDim.toBottom
  }

liftPic : (C.Form -> C.Form) -> (Dim -> Dim) -> Pic -> Pic
liftPic mapForm mapDim picture =
  { asForm = mapForm picture.asForm
  , picSize = mapDim picture.picSize
  }

movePic : (Float, Float) -> Pic -> Pic
movePic offset = liftPic (C.move offset) (moveDim offset)

moveDim : (Float, Float) -> Dim -> Dim
moveDim (offsetx, offsety) dims =
  { toLeft = dims.toLeft + offsetx
  , toTop = dims.toTop + offsety
  , toRight = dims.toRight + offsetx
  , toBottom = dims.toBottom + offsety
  }

padded : Float -> Pic -> Pic
padded padding = liftPic identity (paddedDim padding)

paddedDim : Float -> Dim -> Dim
paddedDim padding dim =
  { toLeft = dim.toLeft - padding
  , toTop = dim.toTop + padding
  , toRight = dim.toRight + padding
  , toBottom = dim.toBottom - padding
  }

scalePic : Float -> Pic -> Pic
scalePic factor = liftPic (C.scale factor) (scaleDim factor)

scaleDim : Float -> Dim -> Dim
scaleDim factor dim =
  { toLeft = factor * dim.toLeft
  , toTop = factor * dim.toTop
  , toRight = factor * dim.toRight
  , toBottom = factor * dim.toBottom
  }

alpha : Float -> Pic -> Pic
alpha a = liftPic (C.alpha (clamp 0 1 a)) identity

withDim : Dim -> Pic -> Pic
withDim dim { asForm } = { asForm = asForm, picSize = dim }

-- CONCRETE

-- origin at center by default
text : T.Text -> Pic
text content =
  let asElement = E.leftAligned content
      w = toFloat (E.widthOf asElement)
      -- for some reason it didnt _quite_ fit,
      -- so i had to play around with some values...
      h' = toFloat (E.heightOf asElement)
      h = h' * (2 / 3) -- actual height... ...
      textSize =
        { toLeft = -w / 2
        , toTop = h / 2
        , toRight = w / 2
        , toBottom = -h / 2
        }
   in { asForm = C.move (0, textSize.toTop * (5/4)) (C.text content)
      , picSize = textSize
      }

-- origin at center
squareDim : Float -> Dim
squareDim radius =
  { toLeft = -radius
  , toTop = radius
  , toRight = radius
  , toBottom = -radius
  }

-- origin at center
circle : Float -> Shape
circle r =
  { elmShape = C.circle r
  , shapeSize = squareDim r
  }

rectFromDim : Dim -> Shape
rectFromDim dim =
  let vertices =
        [ (dim.toLeft, dim.toTop)
        , (dim.toRight, dim.toTop)
        , (dim.toRight, dim.toBottom)
        , (dim.toLeft, dim.toBottom)
        ]
   in { elmShape = C.polygon vertices
      , shapeSize = dim
      }

filled : Color -> Shape -> Pic
filled col shape =
  { asForm = C.filled col shape.elmShape
  , picSize = shape.shapeSize
  }

outlined : C.LineStyle -> Shape -> Pic
outlined lineStyle shape =
  { asForm = C.outlined lineStyle shape.elmShape
  , picSize = shape.shapeSize
  }

solid : Color -> C.LineStyle
solid = C.solid

debugEnvelope : Pic -> Pic
debugEnvelope picture =
  atop pic (outlined (C.solid Color.red) (rectFromDim picture.picSize)) picture

toElement : (Int, Int) -> Pic -> E.Element
toElement (w, h) picture = C.collage w h [ picture.asForm ]
