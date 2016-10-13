module ScoreBoard exposing (..)

import Collage exposing (collage, rect, solid, filled, outlined, text, LineCap(Round), move)
import Element exposing (flow, down, Element)
import Text exposing (fromString, monospace, bold)
import Color


type alias Score =
    { points : Int
    , lines : Int
    , level : Int
    }


newScore : Int -> Score
newScore level =
    { points = 0
    , lines = 0
    , level = level
    }


addLines : Int -> Score -> Score
addLines lines s =
    let
        newLines =
            s.lines + lines

        additionalPoints =
            calculatePoints lines s.level

        newPoints =
            s.points + additionalPoints

        newLevel =
            max s.level (newLines // 10)
    in
        { s
            | lines = newLines
            , points = newPoints
            , level = newLevel
        }


calculatePoints : Int -> Int -> Int
calculatePoints lines level =
    factorForLines lines * (level + 1)


factorForLines : Int -> Int
factorForLines lines =
    case lines of
        1 ->
            40

        2 ->
            100

        3 ->
            300

        4 ->
            1200

        _ ->
            0


view : Score -> Int -> Element
view s width =
    flow down
        [ boxWithText width "Points" s.points
        , boxWithText width "Lines" s.lines
        , boxWithText width "Level" s.level
        ]


boxWithText : Int -> String -> Int -> Element
boxWithText w label value =
    let
        boxShape =
            (rect (toFloat w - 10) 45)

        box =
            filled Color.white boxShape

        lineStyle =
            (solid Color.black)

        lineStyle_ =
            { lineStyle | width = 3.0, cap = Round }

        border =
            outlined lineStyle_ boxShape
    in
        collage w
            50
            [ box
            , border
            , (label ++ ":")
                |> fromString
                |> monospace
                |> Text.height 20
                |> bold
                |> text
                |> move ( 0, 12 )
            , (toString value)
                |> fromString
                |> monospace
                |> Text.height 20
                |> bold
                |> text
                |> move ( 0, -8 )
            ]
