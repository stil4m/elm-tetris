module Controller (..) where

import Graphics.Element exposing (Element, show)
import Keyboard exposing (arrows, presses)
import Signal exposing (Signal)
import Time exposing (Time, fps)
import Char exposing (toCode, KeyCode)
type Input
  = Rotate
  | Shift ( Int, Int )
  | Tick Time
  | ToggleNext

arrowsToInput : { x : Int, y : Int } -> Input
arrowsToInput { x, y } =
  if y == 1 then
    Rotate
  else
    Shift ( y, x )

asCommand : KeyCode -> Input
asCommand k =
  if k == (toCode 'h') then
    ToggleNext
  else
    Tick 0

inputs : Signal Input
inputs =
  let
    ticks = Signal.map Tick (fps 6)
    keys = Signal.map arrowsToInput arrows
    commands = Signal.map asCommand presses
  in
    Signal.mergeMany [ticks,keys, commands]

main : Signal Element
main =
  Signal.map show inputs
