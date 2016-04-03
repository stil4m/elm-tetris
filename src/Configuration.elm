module Configuration (..) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Controller exposing (Input(..), Direction(..))
import Text exposing (..)
import Color


type alias Configuration =
  { level : Int
  , activeElement : Int
  , timestamp : Float
  }


initialConfiguration : Configuration
initialConfiguration =
  { level = 0
  , activeElement = 0
  , timestamp = 0
  }


configElements : Int
configElements =
  1


update : Input -> Configuration -> Configuration
update i configuration =
  case i of
    Direction Down ->
      { configuration | activeElement = min (configElements - 1) (configuration.activeElement + 1) }

    Direction Up ->
      { configuration | activeElement = max 0 (configuration.activeElement - 1) }

    Direction Left ->
      updateCurrentConfigurationItem configuration Left

    Direction Right ->
      updateCurrentConfigurationItem configuration Right

    Frame stamp _ ->
      { configuration | timestamp = stamp }

    _ ->
      configuration


updateCurrentConfigurationItem : Configuration -> Controller.Direction -> Configuration
updateCurrentConfigurationItem configuration dir =
  case ( dir, configuration.activeElement ) of
    ( Left, 0 ) ->
      { configuration | level = max 0 (configuration.level - 1) }

    ( Right, 0 ) ->
      { configuration | level = min 15 (configuration.level + 1) }

    _ ->
      configuration


view : Configuration -> Element
view configuration =
  let
    item =
      (configItem (configuration.activeElement == 0) "Level" (toString <| configuration.level))

    ( w, h ) =
      sizeOf item
  in
    container
      400
      500
      midTop
      (flow
        down
        [ item
        , fromString "Use arrows to set the initial level\nand press enter to start the game" |> leftAligned
        , show configuration.timestamp
        ]
      )


configItem : Bool -> String -> String -> Element
configItem active title value =
  let
    lineStyle =
      if active then
        solid
      else
        dotted

    borderStyle =
      (lineStyle Color.black)

    borderStyle' =
      { borderStyle | width = 3.0 }

    border =
      outlined borderStyle' (rect 180 40)

    rightArrow =
      filled Color.black (ngon 3 5)

    rightArrow' =
      collage 10 40 [ rightArrow ]
        |> toForm
        |> move ( 80, -1 )

    leftArrow =
      collage 10 40 [ (rotate (degrees 180) rightArrow) ]
        |> toForm
        |> move ( 50, -1 )

    arrows =
      if active then
        [ rightArrow', leftArrow ]
      else
        []
  in
    flow
      right
      [ collage
          (180 + 6 + 2 * 20)
          (40 + 6 + 2 * 20)
          [ group
              ([ fromString (title ++ ":")
                  |> leftAligned
                  |> container 100 40 midLeft
                  |> toForm
                  |> move ( -35, 0 )
               , value
                  |> fromString
                  |> centered
                  |> container 20 40 middle
                  |> toForm
                  |> move ( 65, 0 )
               , border
               ]
                ++ arrows
              )
          ]
      ]
