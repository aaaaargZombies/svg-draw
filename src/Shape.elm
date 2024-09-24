module Shape exposing (..)

import Math.Vector2 exposing (Vec2, vec2)
import String exposing (fromFloat, fromInt)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, rx, ry, width, x, y)


type Shape
    = Circle Vec2 Vec2
    | Rectangle Vec2 Vec2


toSvg : Shape -> Svg.Svg msg
toSvg shape =
    case shape of
        Circle start end ->
            let
                x =
                    start |> Math.Vector2.getX |> fromFloat

                y =
                    start |> Math.Vector2.getY |> fromFloat

                r_ =
                    Math.Vector2.distance start end |> fromFloat
            in
            circle
                [ cx x
                , cy y
                , r r_
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
                    xEnd - xStart |> fromFloat

                height_ =
                    yEnd - yStart |> fromFloat
            in
            rect
                [ x <| fromFloat xStart
                , y <| fromFloat yStart
                , width width_
                , height height_
                , rx "5"
                , ry "5"
                ]
                []
