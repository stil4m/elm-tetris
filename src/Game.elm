module Game (..) where

import Board exposing (Board)
import Controller exposing (..)
import Graphics.Collage exposing (collage, group, text)
import Graphics.Element exposing (Element, flow, right, up, midBottom, container, show)
import Random exposing (Generator, Seed)
import Signal exposing (Signal)
import Tetromino exposing (Tetromino)
import Time exposing (Time)
import Upcoming
import Block
import ScoreBoard exposing (Score)


type alias State =
  { falling : Tetromino
  , seed : Seed
  , gameOver : Bool
  , bag : List Tetromino
  , board : Board
  , time : Time
  , nextShift : Time
  , shiftDelay : Time
  , pieceNumber : Int
  , score : Score
  , showNext : Bool
  , paused : Bool
  }


startingShift : ( Int, Int )
startingShift =
  ( 20, 5 )


initialSeed : Int
initialSeed =
  43


defaultState : State
defaultState =
  let
    ( bag, seed ) =
      Random.generate Tetromino.bag (Random.initialSeed initialSeed)

    falling =
      List.head bag |> Maybe.withDefault Tetromino.i

    bag' =
      List.drop 1 bag

    score =
      ScoreBoard.initialScore

    shift =
      asShiftDelay score.level
  in
    { falling = Tetromino.shift startingShift falling
    , seed = seed
    , gameOver = False
    , bag = bag'
    , board = Board.new []
    , time = 0
    , score = score
    , nextShift = shift
    , shiftDelay = shift
    , pieceNumber = 1
    , showNext = True
    , paused = False
    }


view : State -> Element
view state =
  let
    boardWidth =
      round (toFloat Board.cols * Block.size)

    boardHeight =
      round (toFloat Board.rows * Block.size)

    boardForm =
      if state.paused then
        Board.pausedForm
      else if state.gameOver then
        Board.gameOverForm state.board
      else
        Board.addTetromino state.falling state.board |> Board.toForm

    next =
      Maybe.withDefault Tetromino.i (List.head state.bag)

    sideBarWidth =
      (6 * round Block.size)
  in
    flow
      right
      [ collage boardWidth boardHeight [ boardForm ]
      , container sideBarWidth boardHeight midBottom
          <| flow
              up
              [ Upcoming.toElement (not state.paused && state.showNext) next sideBarWidth
              , ScoreBoard.view state.score sideBarWidth
              ]
      , show (toString state.gameOver)
      ]


checkBag : State -> State
checkBag state =
  if not (List.isEmpty state.bag) then
    state
  else
    let
      ( bag, seed ) =
        Random.generate Tetromino.bag state.seed
    in
      { state
        | bag = bag
        , seed = seed
      }


nextTetromino : State -> State
nextTetromino state =
  let
    nextFalling =
      List.head state.bag
        |> Maybe.withDefault Tetromino.i
        |> Tetromino.shift startingShift

    nextBag =
      List.drop 1 state.bag

    ( lines, nextBoard ) =
      Board.addTetromino state.falling state.board
        |> Board.clearLines

    newScore =
      ScoreBoard.addLines lines state.score
  in
    checkBag
      { state
        | falling = nextFalling
        , board = nextBoard
        , bag = nextBag
        , pieceNumber = state.pieceNumber + 1
        , score = newScore
      }


asShiftDelay : Int -> Float
asShiftDelay x =
  ((sqrt (toFloat x) * -1) / sqrt 15 + 1) * 1000


checkTick : State -> State
checkTick state =
  if state.time < state.nextShift then
    state
  else
    let
      shifted =
        Tetromino.shift ( -1, 0 ) state.falling

      nextShift =
        state.time + state.shiftDelay

      isValid =
        Board.isValid shifted state.board

      state' =
        if isValid then
          { state | falling = shifted }
        else
          nextTetromino state

      newShiftDelay =
        asShiftDelay state'.score.level
    in
      { state'
        | nextShift = nextShift
        , shiftDelay = newShiftDelay
      }


useIfValid : State -> State -> State
useIfValid current new =
  if Board.isValid new.falling new.board then
    new
  else
    current


tryKicks : List ( Int, Int ) -> State -> State -> State
tryKicks shifts current nextState =
  case shifts of
    [] ->
      current

    s :: rest ->
      let
        shifted =
          Tetromino.shift s nextState.falling
      in
        if Board.isValid shifted nextState.board then
          { nextState | falling = shifted }
        else
          tryKicks rest current nextState


wallKick : State -> State -> State
wallKick current nextState =
  let
    range =
      nextState.falling.cols // 2

    shifts =
      [1..range] |> List.concatMap (\n -> [ ( 0, n ), ( 0, -1 ) ])
  in
    tryKicks shifts current nextState


floorKick : State -> State -> State
floorKick current nextState =
  let
    range =
      nextState.falling.rows // 2

    shifts =
      [1..range] |> List.map (\n -> ( n, 0 ))
  in
    tryKicks shifts current nextState


update : Input -> State -> State
update input state =
  if state.gameOver then
    updateGameOver input state
  else if state.paused then
    updatePausedGame input state
  else
    updateActiveGame input state

updateGameOver : Input -> State -> State
updateGameOver input state =
  state

updatePausedGame : Input -> State -> State
updatePausedGame input state =
  case input of
    Pause ->
      { state | paused = not state.paused }
    _ ->
      state

updateActiveGame : Input -> State -> State
updateActiveGame input state =
  let
    useIfValid' =
      useIfValid state
  in
    case input of
      Pause ->
        { state | paused = not state.paused }

      Rotate x ->
        let
          rotated =
            { state
              | falling = Tetromino.rotate x state.falling
            }

          nextState =
            useIfValid' rotated

          nextState' =
            if nextState == state then
              wallKick state rotated
            else
              nextState

          nextState'' =
            if nextState' == state then
              floorKick state rotated
            else
              nextState'
        in
          nextState''

      Shift amount ->
        useIfValid'
          { state
            | falling = Tetromino.shift amount state.falling
          }

      Tick delta ->
        let
          newState =
            checkTick
              { state
                | time = state.time + delta
              }

          valid =
            Board.isValid newState.falling newState.board
        in
          { newState | gameOver = not valid }

      ToggleNext ->
        { state | showNext = not state.showNext }


states : Signal State
states =
  Signal.foldp update defaultState inputs


main : Signal Element
main =
  Signal.map view states