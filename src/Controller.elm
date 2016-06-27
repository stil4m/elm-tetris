module Controller exposing (..)

import Keyboard exposing (presses, KeyCode)
import Time exposing (Time)


type Direction
    = Up
    | Down
    | Right
    | Left
    | None


type Input
    = Direction Direction
    | Key KeyCode
    | Frame Time Time
    | Enter


type alias Model =
    Maybe Time


type Msg
    = PressDown KeyCode
    | Tick Time


update : Msg -> Model -> ( Model, Input )
update msg model =
    case msg of
        PressDown k ->
            if (k == 37) then
                ( model, Direction Left )
            else if (k == 38) then
                ( model, Direction Up )
            else if (k == 39) then
                ( model, Direction Right )
            else if (k == 40) then
                ( model, Direction Down )
            else if (k == 13) then
                ( model, Enter )
            else
                ( model, Key k )

        Tick t ->
            case model of
                Just old ->
                    let
                        interval =
                            t - old
                    in
                        ( Just t, Frame t interval )

                Nothing ->
                    ( Just t, Frame t 0 )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Keyboard.downs PressDown
        , Time.every (1000.0 / 33) Tick
        ]
