module Controller (..) where

import Keyboard exposing (arrows, presses)
import Signal exposing (Signal)
import Time exposing (Time, fps)
import Char exposing (KeyCode, toCode)

type Direction
  = Up
  | Down
  | Right
  | Left
  | None


type Input
  = Direction Direction
  | Key KeyCode
  | Frame Time
  | Enter


iff : a -> a -> (Bool -> a)
iff x y = (\a -> if a then x else y)
inputs : Signal Input
inputs =
  let
    ticks =
      Signal.map Frame (fps 30)

    keys =
      arrows
        |> Signal.foldp realArrows ( { x = 0, y = 0 }, None )
        |> Signal.map snd
        |> Signal.map Direction

    commands =
      presses
        |> Signal.filter (\x -> x /= 13) 0
        |> Signal.map Key

    enters =
      Keyboard.enter
      |> Signal.map (iff Enter (Frame 0))

  in
    Signal.mergeMany [ ticks, keys, commands, enters]


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
