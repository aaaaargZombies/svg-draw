module Shape exposing (..)

import Math.Vector2 exposing (Vec2)
import String
import Svg exposing (Svg)
import Svg.Attributes


type Shape
    = Circle Vec2 Vec2
    | Rectangle Vec2 Vec2


toSvg : Shape -> Svg msg
toSvg shape =
    case shape of
        Circle start end ->
            let
                x =
                    start |> Math.Vector2.getX |> String.fromFloat

                y =
                    start |> Math.Vector2.getY |> String.fromFloat

                r_ =
                    Math.Vector2.distance start end |> String.fromFloat
            in
            Svg.circle
                [ Svg.Attributes.cx x
                , Svg.Attributes.cy y
                , Svg.Attributes.r r_
                ]
                []

        Rectangle start end ->
            let
                xStart =
                    min (Math.Vector2.getX start) (Math.Vector2.getX end)

                yStart =
                    min (Math.Vector2.getY start) (Math.Vector2.getY end)

                xEnd =
                    max (Math.Vector2.getX start) (Math.Vector2.getX end)

                yEnd =
                    max (Math.Vector2.getY start) (Math.Vector2.getY end)

                width_ =
                    xEnd - xStart |> String.fromFloat

                height_ =
                    yEnd - yStart |> String.fromFloat
            in
            Svg.rect
                [ Svg.Attributes.x <| String.fromFloat xStart
                , Svg.Attributes.y <| String.fromFloat yStart
                , Svg.Attributes.width width_
                , Svg.Attributes.height height_
                , Svg.Attributes.rx "5"
                , Svg.Attributes.ry "5"
                ]
                []
