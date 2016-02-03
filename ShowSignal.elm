module ShowSignal where

import ExtraSignals exposing (..)
import Graphics.Element exposing (..)
import Text exposing (Text)

main : Signal Element
main = testSignal fingerPosition

testSignal : Signal a -> Signal Element
testSignal signal = Signal.map (leftAligned << Text.fromString) (Signal.foldp update "" signal)

update : event -> String -> String
update event string = string ++ "\n" ++ toString event
