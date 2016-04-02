module GameController where

import Controller exposing (..)
import Time exposing (Time)
import Char exposing (toCode, KeyCode)

type GameInput
  = Rotate Bool
  | Shift ( Int, Int )
  | Tick Time
  | ToggleNext
  | Pause

inputToGameInput : Input -> GameInput
inputToGameInput i =
  case i of
    Direction d ->
      arrowsToGameInput d
    Key k ->
      keyToGameInput k
    Frame f ->
      Tick f


keyToGameInput : KeyCode -> GameInput
keyToGameInput k =
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

arrowsToGameInput : Direction -> GameInput
arrowsToGameInput d =
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
