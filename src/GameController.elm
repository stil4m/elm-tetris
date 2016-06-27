module GameController exposing (inputToGameInput, GameInput(..))

import Controller exposing (Input(..), Direction(..))
import Time exposing (Time)
import Char exposing (KeyCode, fromCode, toLower)


type GameInput
    = Rotate Bool
    | Shift ( Int, Int )
    | Tick Time
    | ToggleNext
    | Pause
    | NoOp


inputToGameInput : Input -> GameInput
inputToGameInput i =
    case i of
        Direction d ->
            arrowsToGameInput d

        Key k ->
            keyToGameInput k

        Frame stamp f ->
            Tick f

        Enter ->
            NoOp


keyToGameInput : KeyCode -> GameInput
keyToGameInput k =
    let
        c =
            Debug.log "C:" (k |> Char.fromCode |> Char.toLower)
    in
        if c == 'h' then
            ToggleNext
        else if c == 'n' then
            Rotate False
        else if c == 'm' then
            Rotate True
        else if c == 'p' then
            Pause
        else
            NoOp


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
            NoOp
