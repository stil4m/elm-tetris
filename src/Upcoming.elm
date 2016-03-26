module Upcoming (..) where

import Graphics.Element exposing (Element)
import Graphics.Collage exposing (collage, square, solid, filled, outlined, move)
import Color
import Tetromino exposing (Tetromino)
import Block


toElement : Bool -> Tetromino -> Element
toElement showNext tetromino =
  let
    dim =
      (6 * round Block.size)

    box =
      (square (toFloat dim - 10.0))

    border =
      (solid Color.black)

    border' =
      { border | width = 3.0 }

    forms =
      [ filled Color.white box
      , outlined border' box
      ]

    forms' =
      if showNext then
        forms
          ++ [ move ( -tetromino.pivot.c * Block.size, -tetromino.pivot.r * Block.size )
                <| Tetromino.toForm
                <| tetromino
             ]
      else
        forms
  in
    collage dim dim forms'
