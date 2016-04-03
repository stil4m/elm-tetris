module Tetris (..) where

import Game exposing (Game)
import Configuration exposing (Configuration)
import Controller exposing (inputs, Input(..))
import GameController exposing (..)
import Graphics.Element exposing (Element, color)
import Color
import Html exposing (Html, div, text, fromElement)
import Html.Attributes exposing (style)


type alias Tetris =
  { configuration : Configuration
  , game : Maybe Game
  , windowSize : ( Int, Int )
  }


defaultTetris : Tetris
defaultTetris =
  { game = Nothing
  , configuration = Configuration.initialConfiguration
  , windowSize = ( 0, 0 )
  }


update : Input -> Tetris -> Tetris
update input tetris =
  case tetris.game of
    Just game ->
      let
        newGame =
          (Game.update (inputToGameInput input) game)

        newGame' =
          if newGame.gameOver && (input == Enter) then
            Nothing
          else
            Just newGame
      in
        { tetris | game = newGame' }

    Nothing ->
      let
        newConfig =
          Configuration.update input tetris.configuration

        startGame =
          input == Enter

        newGame =
          if startGame then
            Just (Game.newGame newConfig.level newConfig.timestamp)
          else
            Nothing
      in
        { tetris | configuration = newConfig, game = newGame }



-- view : Tetris -> Element


view : Tetris -> Html
view tetris =
  let
    inner =
      case tetris.game of
        Just game ->
          Game.view game

        Nothing ->
          Configuration.view tetris.configuration
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
          [ fromElement (inner |> color Color.white) ]
      ]


states : Signal Tetris
states =
  Signal.foldp update defaultTetris inputs


main : Signal Html
main =
  Signal.map view states
