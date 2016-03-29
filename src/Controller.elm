module Controller (..) where

import Keyboard exposing (arrows, presses)
import Signal exposing (Signal)
import Time exposing (Time, fps)
import Char exposing (toCode, KeyCode)


type Direction
  = Up
  | Down
  | Right
  | Left
  | None


type Input
  = Rotate Bool
  | Shift ( Int, Int )
  | Tick Time
  | ToggleNext
  | Pause


arrowsToInput : Direction -> Input
arrowsToInput d =
  case d of
    Left ->
      Shift ( 0, -1 )

    Right ->
      Shift ( 0, 1 )

    Down ->
      Shift ( -1, 0 )

    Up ->
      Tick 0

    None ->
      Tick 0


asCommand : KeyCode -> Input
asCommand k =
  if k == (toCode 'h') then
    ToggleNext
  else if k == (toCode 'n') then
    Rotate False
  else if k == (toCode 'm') then
    Rotate True
  else if k == (toCode 'p') then
      Pause
  else
    Tick 0


inputs : Signal Input
inputs =
  let
    ticks =
      Signal.map Tick (fps 30)

    keys =
      Signal.foldp realArrows ( { x = 0, y = 0 }, None ) arrows
        |> Signal.map snd
        |> Signal.map arrowsToInput

    commands =
      Signal.map asCommand presses
  in
    Signal.mergeMany [ ticks, keys, commands ]


realArrows : { x : Int, y : Int } -> ( { x : Int, y : Int }, Direction ) -> ( { x : Int, y : Int }, Direction )
realArrows newArrows ( oldArrows, oldDirection ) =
  if newArrows.x == -1 && oldArrows.x /= -1 then
    ( newArrows, Left )
  else if newArrows.x == 1 && oldArrows.x /= 1 then
    ( newArrows, Right )
  else if newArrows.y == 1 && oldArrows.y /= 1 then
    ( newArrows, Up )
  else if newArrows.y == -1 && oldArrows.y /= -1 then
    ( newArrows, Down )
  else
    ( newArrows, None )
