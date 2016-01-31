module Pic where

import Graphics.Element as E
import Graphics.Collage as C
import Text as T
import Color exposing (Color)

type alias Dim =
  { toLeft : Float
  , toTop : Float
  , toRight : Float
  , toBottom : Float
  }
type alias Pic =
  { asForm : C.Form
  , picSize : Dim
  }
type alias Shape =
  { elmShape : C.Shape
  , shapeSize : Dim
  }

text : T.Text -> Pic
text content =
  let asElement = E.leftAligned content
      textSize =
        { toLeft = 0
        , toTop = 0
        , toRight = toFloat (E.widthOf asElement)
        , toBottom = toFloat (E.heightOf asElement)
        }
   in { asForm = C.text content
      , picSize = textSize
      }

atop : Pic -> Pic -> Pic
atop picAtop picBelow =
  { asForm = C.group [ picBelow.asForm, picAtop.asForm ]
  , picSize = atopDims picAtop.picSize picBelow.picSize
  }

atopDims : Dim -> Dim -> Dim
atopDims atop below =
  { toLeft = min atop.toLeft below.toLeft
  , toTop = min atop.toTop below.toTop
  , toRight = max atop.toRight below.toRight
  , toBottom = max atop.toBottom below.toBottom
  }

move : (Float, Float) -> Pic -> Pic
move offset pic =
  { asForm = C.move offset pic.asForm
  , picSize = moveDim offset pic.picSize
  }

moveDim : (Float, Float) -> Dim -> Dim
moveDim (offsetx, offsety) dims =
  { toLeft = dims.toLeft + offsetx
  , toTop = dims.toTop + offsety
  , toRight = dims.toRight + offsetx
  , toBottom = dims.toBottom + offsety
  }

circle : Float -> Shape
circle r =
  { elmShape = C.circle r
  , shapeSize = Dim r r r r
  }

filled : Color -> Shape -> Pic
filled col shape =
  { asForm = C.filled col shape.elmShape
  , picSize = shape.shapeSize
  }

toElement : (Int, Int) -> Pic -> E.Element
toElement (w, h) picture = C.collage w h [ picture.asForm ]
