module TestPic where

import Graphics.Collage exposing (solid)
import Graphics.Element exposing (Element)
import Pic exposing (..)
import Color
import Text exposing (Text)
import Window

renderBoundingBox : Pic -> Pic
renderBoundingBox picture =
  (outlined (solid Color.red) (rectFromDim picture.picSize)) `atop` picture

buttonWith : Pic -> Pic
buttonWith inner =
  let backgroundRect = rectFromDim (padded 6 inner).picSize
   in inner `atop` (filled (Color.lightBlue) backgroundRect)

button : String -> Pic
button content =
  buttonWith (scale 1.5 <| text <| Text.bold <| Text.color Color.white <| Text.fromString content)

testPic : Pic -> Signal Element
testPic picture =
  let makeElement winSize = toElement winSize picture
   in Signal.map makeElement Window.dimensions

main : Signal Element
main = testPic (button "Test Button")
