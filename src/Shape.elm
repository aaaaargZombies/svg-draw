module Shape exposing (..)

import Math.Vector2 exposing (Vec2)
import String
import Svg exposing (Svg)
import Svg.Attributes


type Shape
    = Circle Vec2 Vec2
    | Rectangle Vec2 Vec2
    | Square Vec2 Vec2


type alias Dimensions =
    { xStart : Float
    , xEnd : Float
    , yStart : Float
    , yEnd : Float
    , width : Float
    , height : Float
    }


dimensions : Vec2 -> Vec2 -> Dimensions
dimensions start end =
    let
        xStart =
            min (Math.Vector2.getX start) (Math.Vector2.getX end)

        yStart =
            min (Math.Vector2.getY start) (Math.Vector2.getY end)

        xEnd =
            max (Math.Vector2.getX start) (Math.Vector2.getX end)

        yEnd =
            max (Math.Vector2.getY start) (Math.Vector2.getY end)

        width =
            xEnd - xStart

        height =
            yEnd - yStart
    in
    { xStart = xStart
    , xEnd = xEnd
    , yStart = yStart
    , yEnd = yEnd
    , width = width
    , height = height
    }


toSvg : Shape -> Svg msg
toSvg shape =
    case shape of
        Circle start end ->
            let
                x =
                    start |> Math.Vector2.getX |> String.fromFloat

                y =
                    start |> Math.Vector2.getY |> String.fromFloat

                r =
                    Math.Vector2.distance start end |> String.fromFloat
            in
            Svg.circle
                [ Svg.Attributes.cx x
                , Svg.Attributes.cy y
                , Svg.Attributes.r r
                ]
                []

        Rectangle start end ->
            let
                d =
                    dimensions start end
            in
            Svg.rect
                [ Svg.Attributes.x <| String.fromFloat d.xStart
                , Svg.Attributes.y <| String.fromFloat d.yStart
                , Svg.Attributes.width <| String.fromFloat d.width
                , Svg.Attributes.height <| String.fromFloat d.height
                , Svg.Attributes.rx "5"
                , Svg.Attributes.ry "5"
                ]
                []

        Square start end ->
            let
                d =
                    dimensions start end

                size =
                    max d.width d.height
            in
            Svg.rect
                [ Svg.Attributes.x <| String.fromFloat d.xStart
                , Svg.Attributes.y <| String.fromFloat d.yStart
                , Svg.Attributes.width <| String.fromFloat size
                , Svg.Attributes.height <| String.fromFloat size
                , Svg.Attributes.rx "5"
                , Svg.Attributes.ry "5"
                ]
                []
