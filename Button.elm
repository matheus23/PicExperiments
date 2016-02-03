module Button where

import Reactive exposing (Reactive, Event(..), TouchType(..))
import Pic exposing (Pic)
import Color
import App exposing (..)
import Text exposing (Text)

type State = Pressed | Normal

type Action = Press | Release

anyButton : Pic -> App State Action
anyButton content =
  let updateButton action state =
        case action of
          Press -> Pressed
          Release -> Normal

      viewButton state =
        Reactive.onEvent createAction
          (Reactive.static (Pic.atop content (background state content)))

      createAction event =
        case event of
          (TouchEvent FingerDown _) -> Just Press
          (TouchEvent FingerUp _) -> Just Release
          otherwise -> Nothing

      background state inner =
        case state of
          Normal -> Pic.padded 4 <| Pic.filled (Color.lightBlue) (Pic.rectFromDim (Pic.padded 10 inner).picSize)
          Pressed ->
            Pic.atop
              (Pic.filled (Color.lightBlue) (Pic.rectFromDim (Pic.padded 10 inner).picSize))
              (Pic.filled (Color.black) (Pic.rectFromDim (Pic.padded 14 inner).picSize))

   in { init = Normal
      , update = updateButton
      , view = viewButton
      }

textButton : String -> App State Action
textButton label =
  anyButton (Pic.scale 2 <| Pic.text <| Text.color Color.white <| Text.bold <| Text.fromString label)
