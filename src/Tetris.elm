module Tetris where

import Game exposing (Game)
import Configuration exposing (Configuration)
import Controller exposing (inputs, Input(..))
import GameController exposing (..)
import Graphics.Element  exposing (Element)

type alias Tetris =
   { configuration : Configuration
   , game : Maybe Game
   }

defaultTetris : Tetris
defaultTetris =
  { game = Nothing --Just Game.defaultState
  , configuration = Configuration.initialConfiguration
  }

update : Input -> Tetris -> Tetris
update input tetris =
  case tetris.game of
    Just game ->
      let
        newGame = (Game.update (inputToGameInput input) game)
        newGame' =
          if newGame.gameOver && (input == Enter)  then
            Nothing
          else
            Just newGame
      in
        { tetris | game = newGame' }
    Nothing ->
      let
        newConfig = Configuration.update input tetris.configuration
        startGame = input == Enter
        newGame =
          if startGame then
            Just (Game.newGame newConfig.level)
          else
            Nothing
      in
        { tetris | configuration = newConfig, game = newGame}

view : Tetris -> Element
view tetris =
  case tetris.game of
    Just game ->  Game.view game
    Nothing ->
      Configuration.view tetris.configuration

states : Signal Tetris
states =
  Signal.foldp update defaultTetris inputs

main : Signal Element
main =
   Signal.map view states
