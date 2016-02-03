module ExtraSignals where

import Touch exposing (Touch)

fingerPressed : Signal ()
fingerPressed =
  let onlyDown isDown = if isDown then Just () else Nothing
   in Signal.filterMap onlyDown () isFingerPressed

fingerReleased : Signal ()
fingerReleased =
  let onlyUp isDown = if not isDown then Just () else Nothing
   in Signal.filterMap onlyUp () isFingerPressed

isFingerPressed : Signal Bool
isFingerPressed =
  Signal.dropRepeats (Signal.map (not << List.isEmpty) Touch.touches)

firstTouch : Signal Touch
firstTouch = Signal.dropRepeats (Signal.filterMap List.head (Touch 0 0 0 0 0 0) Touch.touches)

fingerPosition : Signal (Float, Float)
fingerPosition =
  let getPosition { x, y } = (toFloat x, toFloat y)
   in Signal.map getPosition firstTouch
