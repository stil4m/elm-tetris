module Tetris exposing (..)

import Game exposing (Game)
import Configuration exposing (Configuration)
import Controller exposing (subscriptions)
import GameController exposing (..)
import Element exposing (Element, color, toHtml)
import Color
import Html exposing (Html, div, text, program)
import Html.Attributes exposing (style)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    defaultTetris ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Controller.subscriptions |> Sub.map ControllerMsg


type alias Model =
    Tetris


type alias Tetris =
    { configuration : Configuration
    , game : Maybe Game
    , windowSize : ( Int, Int )
    , controller : Controller.Model
    }


type Msg
    = ControllerMsg Controller.Msg


defaultTetris : Tetris
defaultTetris =
    { game = Nothing
    , configuration = Configuration.initialConfiguration
    , controller = Nothing
    , windowSize = ( 0, 0 )
    }


update : Msg -> Tetris -> ( Tetris, Cmd Msg )
update msg model =
    case Debug.log "Msg" msg of
        ControllerMsg x ->
            let
                ( newController, input ) =
                    Controller.update x model.controller

                tetris =
                    { model | controller = newController }
            in
                case tetris.game of
                    Just game ->
                        let
                            newGame =
                                (Game.update (GameController.inputToGameInput input) game)

                            newGame_ =
                                if newGame.gameOver && (input == Controller.Enter) then
                                    Nothing
                                else
                                    Just newGame
                        in
                            { tetris | game = newGame_ } ! []

                    Nothing ->
                        let
                            newConfig =
                                Configuration.update input tetris.configuration

                            startGame =
                                input == Controller.Enter

                            newGame =
                                if startGame then
                                    Just (Game.newGame newConfig.level newConfig.timestamp)
                                else
                                    Nothing
                        in
                            { tetris | configuration = newConfig, game = newGame } ! []


view : Tetris -> Html Msg
view tetris =
    let
        inner =
            tetris.game
                |> Maybe.map Game.view
                |> Maybe.withDefault (Configuration.view tetris.configuration)
                |> color Color.white
                |> toHtml
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "background", "#ccc" )
                , ( "top", "0" )
                , ( "bottom", "0" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "text-align", "center" )
                ]
            ]
            [ div
                [ style
                    [ ( "position", "relative" )
                    , ( "margin", "10px auto" )
                    , ( "display", "inline-block" )
                    ]
                ]
                [ inner ]
            ]
