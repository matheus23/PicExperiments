module Button where

import Reactive exposing (Reactive, Event(..), TouchType(..))
import Pic exposing (Pic)
import Color
import App exposing (..)
import Text exposing (Text)

type State = Pressed | Normal

type Action = Press | Release Bool

anyButton : Pic -> App State Action
anyButton content =
  let updateButton action state =
        case action of
          Press -> Pressed
          Release releasedInside -> Normal

      viewPic state = Pic.atop content (background state content)

      viewButton state =
        Reactive.onEvent (createAction (viewPic state))
          (Reactive.static (viewPic state))

      createAction pic event =
        case event of
          (TouchEvent FingerDown pos) -> Just Press
          (TouchEvent FingerUp pos) -> Just (Release (Reactive.isInside pos pic.picSize))
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

onPress : message -> Reactive Action -> Reactive (Action, Maybe message)
onPress message buttonReactive =
  let
    makeReaction action =
      case action of
        Release True -> (Release True, Just message)
        other -> (other, Nothing)
   in
    Reactive.alwaysForwardMessage makeReaction buttonReactive
