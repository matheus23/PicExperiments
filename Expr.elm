module Expr where

import Graphics.Element exposing (Element)
import Pic exposing (Pic, pic)
import Dim exposing (..)
import PicLike exposing (..)
import Text exposing (Text)
import App exposing (App)
import Reactive exposing (Reactive, Event(..), reactive)
import Color
import Expr.LitInt as LitInt

type Expr
  = Hole
  | Apply Expr (List Expr)
  | LitInt LitInt.Model

initApply : Expr
initApply = Apply Hole [Hole]

defaultText : String -> Pic
defaultText = scale pic 3 << Pic.text << Text.fromString

viewExpr : Expr -> Reactive Expr
viewExpr expr =
  case expr of
    Hole -> viewHole
    LitInt model -> Reactive.alwaysForwardMessage LitInt (LitInt.view model)
    Apply func args -> viewApply func args

viewHole : Reactive Expr
viewHole =
  let
    radius = 20
    asPic = Pic.outlined (Pic.solid Color.darkGrey) <| Pic.rectFromDim <| Pic.squareDim radius
   in
    Reactive.onFingerDown (always <| Just initApply) <| Reactive.static asPic
    --Reactive.onFingerDown (always <| Just (LitInt (LitInt.fromInt 0))) <| Reactive.static asPic

viewApply : Expr -> List Expr -> Reactive Expr
viewApply func args =
  let
    reactFunc newFunc = Apply newFunc args
    replaceIndex index list replacement =
      List.indexedMap
        (\idx listElem -> if idx == index then replacement else listElem)
        list
    createArg index expr =
      Reactive.alwaysForwardMessage (Apply func << replaceIndex index args)
      <| Reactive.padded 4 <| viewExpr expr

    leftParens = Reactive.static (defaultText "(")
    rightParens = Reactive.static (defaultText ")")

    parens react =
      nextTo reactive Left (nextTo reactive Right react rightParens) leftParens

    plusButton =
      Reactive.onFingerDown (always <| Just <| Apply func (args ++ [Hole]))
        <| Reactive.static (Pic.filled Color.red (Pic.circle 20))
   in
    centered reactive
      (parens
        (appendTo reactive Right
          (Reactive.padded 4 <| Reactive.alwaysForwardMessage reactFunc <| viewExpr func)
          (List.indexedMap createArg args ++ [plusButton])))


evaluate : Expr -> Pic
evaluate expr = defaultText (toString expr)

view : Expr -> Reactive Expr
view expr =
  appendTo reactive Down (viewExpr expr)
    [ Reactive.static <| Pic.padded 10 <| Pic.filled Color.darkGrey <| Pic.rectFromDim (Dim -300 1 300 -1)
    , Reactive.static <| evaluate expr
    ]


main : Signal Element
main = App.run
  { init = Hole
  , update = always -- one does not simply "need update"
  , view = view
  }
