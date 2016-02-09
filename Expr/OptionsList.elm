module Expr.OptionsList where

import Pic exposing (Pic, pic)
import Dim exposing (..)
import PicLike exposing (..)
import Reactive exposing (Reactive, Event(..), reactive)

view : List (Pic, model) -> Reactive model
view optionsAndModels =
  let
    add (picture, model) =
      Reactive.onFingerDown (always <| Just model) <| Reactive.static picture

    options = List.map add optionsAndModels
   in
    centered reactive (appendTo reactive Down (empty reactive) options)
