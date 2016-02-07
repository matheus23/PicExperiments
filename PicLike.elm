module PicLike where

import Dim exposing (..)

type alias PicLike a =
  { move_ : (Float, Float) -> a -> a
  , scale_ : Float -> a -> a
  , atop_ : a -> a -> a
  , empty_ : a
  , dimensions_ : a -> Dim
  }

move : PicLike pic -> (Float, Float) -> pic -> pic
move = .move_

scale : PicLike pic -> Float -> pic -> pic
scale = .scale_

atop : PicLike pic -> pic -> pic -> pic
atop = .atop_

empty : PicLike pic -> pic
empty = .empty_

dimensions : PicLike pic -> pic -> Dim
dimensions = .dimensions_

atopAll : PicLike pic -> List pic -> pic
atopAll p pics =
  case pics of
    [] -> empty p
    (elem :: rest) ->
      atop p elem (atopAll p rest)

appendTo : PicLike pic -> Direction -> pic -> List pic -> pic
appendTo p inDirection reference pics =
  case pics of
    [] -> reference
    (pic :: picsRest) -> nextTo p inDirection reference (appendTo p inDirection pic picsRest)

nextTo : PicLike pic -> Direction -> pic -> pic -> pic
nextTo p inDirection reference picture = atop p (moveNextTo p inDirection reference picture) reference

moveNextTo : PicLike pic -> Direction -> pic -> pic -> pic
moveNextTo p inDirection reference picture =
  move p (offsetNextTo p inDirection reference picture) picture

offsetNextTo : PicLike pic -> Direction -> pic -> pic -> (Float, Float)
offsetNextTo p inDirection reference picture =
  let to (a, b) (u, v) = (u - a, v - b)
      touchingPointRef = toBorderInDir inDirection (dimensions p reference)
      touchingPointPic = toBorderInDir (opposite inDirection) (dimensions p picture)
   in touchingPointPic `to` touchingPointRef

alignX : PicLike pic -> Float -> pic -> pic
alignX p alignment picture =
  let xoffset = (width (dimensions p picture)) * alignment - (dimensions p picture).toRight
   in move p (xoffset, 0) picture

alignY : PicLike pic -> Float -> pic -> pic
alignY p alignment picture =
  let yoffset = (height (dimensions p picture)) * alignment - (dimensions p picture).toTop
   in move p (0, yoffset) picture

centered : PicLike pic -> pic -> pic
centered p picture = alignX p 0.5 (alignY p 0.5 picture)
