module Expr where

import Graphics.Element exposing (Element)
import Pic exposing (Pic, Direction(..))
import Text exposing (Text)
import App exposing (App)
import Reactive exposing (Reactive, Event(..))
import Color
import Expr.LitInt as LitInt

type Expr
  = Hole
  | LitInt LitInt.Model

type Action
  = SubstituteHole Expr -- action on hole
  | OnLitInt LitInt.Action

defaultText : String -> Pic
defaultText = Pic.scale 3 << Pic.text << Text.fromString

viewExpr : Expr -> Reactive Action
viewExpr expr =
  case expr of
    Hole -> viewHole
    LitInt model -> Reactive.alwaysForwardMessage OnLitInt (LitInt.view model)

viewHole : Reactive Action
viewHole =
  let
    radius = 20
    asPic = Pic.outlined (Pic.solid Color.darkGrey) <| Pic.rectFromDim <| Pic.squareDim radius
   in
    Reactive.onFingerDown (always <| Just <| SubstituteHole (LitInt (LitInt.fromInt 0))) <| Reactive.static asPic

evaluate : Expr -> Pic
evaluate expr =
  case expr of
    Hole -> defaultText "<hole>"
    LitInt int -> defaultText (toString int ++ " : Int")

update : Action -> Expr -> Expr
update action expr =
  case action of
    SubstituteHole anotherExpr ->
      case expr of
        Hole -> anotherExpr
        otherwise -> expr
    OnLitInt action ->
      case expr of
        LitInt model -> LitInt <| LitInt.update action model
        otherwise -> expr

view : Expr -> Reactive Action
view expr =
  Reactive.appendTo Down (viewExpr expr)
    [ Reactive.static <| Pic.padded 10 <| Pic.filled Color.darkGrey <| Pic.rectFromDim (Pic.Dim -300 1 300 -1)
    , Reactive.static <| evaluate expr
    ]


main : Signal Element
main = App.run
  { init = Hole
  , update = update
  , view = view
  }
