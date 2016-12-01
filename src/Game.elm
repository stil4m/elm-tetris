module Game exposing (..)

import Board exposing (Board)
import GameController exposing (GameInput(..))
import Collage exposing (collage, group, text)
import Element exposing (Element, flow, right, up, midBottom, container, show)
import Random exposing (Generator, Seed)
import Tetromino exposing (Tetromino)
import Time exposing (Time)
import Upcoming
import Block
import ScoreBoard exposing (Score)


type alias Game =
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


newGame : Int -> Float -> Game
newGame intitialLevel time =
    let
        ( bag, seed ) =
            Random.step Tetromino.bag (Random.initialSeed (round time))

        falling =
            List.head bag |> Maybe.withDefault Tetromino.i

        bag_ =
            List.drop 1 bag

        score =
            ScoreBoard.newScore intitialLevel

        shift =
            asShiftDelay score.level
    in
        { falling = Tetromino.shift startingShift falling
        , seed = seed
        , gameOver = False
        , bag = bag_
        , board = Board.new []
        , time = 0
        , score = score
        , nextShift = shift
        , shiftDelay = shift
        , pieceNumber = 1
        , showNext = True
        , paused = False
        }


view : Game -> Element
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
        flow right
            [ collage boardWidth boardHeight [ boardForm ]
            , container sideBarWidth boardHeight midBottom <|
                flow up
                    [ Upcoming.toElement (not state.paused && state.showNext) next sideBarWidth
                    , ScoreBoard.view state.score sideBarWidth
                    ]
            ]


checkBag : Game -> Game
checkBag state =
    if not (List.isEmpty state.bag) then
        state
    else
        let
            ( bag, seed ) =
                Random.step Tetromino.bag state.seed
        in
            { state
                | bag = bag
                , seed = seed
            }


nextTetromino : Game -> Game
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


checkTick : Game -> Game
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

            state_ =
                if isValid then
                    { state | falling = shifted }
                else
                    nextTetromino state

            newShiftDelay =
                asShiftDelay state_.score.level
        in
            { state_
                | nextShift = nextShift
                , shiftDelay = newShiftDelay
            }


useIfValid : Game -> Game -> Game
useIfValid current new =
    if Board.isValid new.falling new.board then
        new
    else
        current


tryKicks : List ( Int, Int ) -> Game -> Game -> Game
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


wallKick : Game -> Game -> Game
wallKick current nextState =
    let
        range =
            nextState.falling.cols // 2

        shifts =
            List.range 1 range |> List.concatMap (\n -> [ ( 0, n ), ( 0, -1 ) ])
    in
        tryKicks shifts current nextState


floorKick : Game -> Game -> Game
floorKick current nextState =
    let
        range =
            nextState.falling.rows // 2

        shifts =
            List.range 1 range |> List.map (\n -> ( n, 0 ))
    in
        tryKicks shifts current nextState


update : GameInput -> Game -> Game
update input state =
    if state.gameOver then
        updateGameOver input state
    else if state.paused then
        updatePausedGame input state
    else
        updateActiveGame input state


updateGameOver : GameInput -> Game -> Game
updateGameOver input state =
    state


updatePausedGame : GameInput -> Game -> Game
updatePausedGame input state =
    case input of
        Pause ->
            { state | paused = not state.paused }

        _ ->
            state


updateActiveGame : GameInput -> Game -> Game
updateActiveGame input state =
    let
        useIfValid_ =
            useIfValid state
    in
        case input of
            NoOp ->
                state

            Pause ->
                { state | paused = not state.paused }

            Rotate x ->
                let
                    rotated =
                        { state
                            | falling = Tetromino.rotate x state.falling
                        }

                    nextState =
                        useIfValid_ rotated

                    nextState_ =
                        if nextState == state then
                            wallKick state rotated
                        else
                            nextState

                    nextState__ =
                        if nextState_ == state then
                            floorKick state rotated
                        else
                            nextState_
                in
                    nextState__

            Shift amount ->
                useIfValid_
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
