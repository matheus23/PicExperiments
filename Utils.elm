module Utils where

import Pic exposing (Pic, pic)
import Dim exposing (..)
import PicLike exposing (..)
import Text exposing (Text)

defaultText : String -> Pic
defaultText = scale pic 3 << Pic.text << Text.fromString

withName : String -> Pic -> Pic
withName name visual =
  nextTo pic Right
    (alignX pic 0 (defaultText name))
    visual

allJust : List (Maybe a) -> Maybe (List a)
allJust list =
  case list of
    [] ->
      Just []

    (Just x :: xs) ->
      case allJust xs of
        Just ls ->
          Just (x :: ls)

        Nothing ->
          Nothing

    otherwise ->
      Nothing
