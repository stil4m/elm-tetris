module Configuration exposing (..)

import Element exposing (..)
import Collage exposing (..)
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
        collage 400
            500
            [ container 300
                500
                topLeft
                (flow down
                    [ item
                    , fromString "Use arrows to set the initial level and\npress enter to start the game"
                        |> leftAligned
                        |> toForm
                        |> move ( 0, 0 )
                        |> (\x -> collage 300 50 [ x ])
                    ]
                )
                |> toForm
            ]


configItemBorderWidth : Float
configItemBorderWidth =
    3.0


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

        borderStyle_ =
            { borderStyle | width = configItemBorderWidth }

        border =
            outlined borderStyle_ (rect (300 - 2 * configItemBorderWidth) (40 - 2 * configItemBorderWidth))

        rightArrow =
            filled Color.black (ngon 3 5)

        rightArrow_ =
            collage 10 40 [ rightArrow ]
                |> toForm
                |> move ( 130, -1 )

        leftArrow =
            collage 10 40 [ (rotate (degrees 180) rightArrow) ]
                |> toForm
                |> move ( 100, -1 )

        arrows =
            if active then
                [ rightArrow_, leftArrow ]
            else
                []
    in
        collage 300
            40
            [ group
                ([ fromString (title ++ ":")
                    |> leftAligned
                    |> container 100 40 midLeft
                    |> toForm
                    |> move ( -85, 0 )
                 , value
                    |> fromString
                    |> centered
                    |> container 20 40 middle
                    |> toForm
                    |> move ( 115, 0 )
                 , border
                 ]
                    ++ arrows
                )
            ]
