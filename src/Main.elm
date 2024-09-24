port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Html exposing (Html, button, div, main_, text)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import Math.Vector2 exposing (Vec2, vec2)
import String exposing (fromFloat, fromInt)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, r, rx, ry, viewBox, x, y)
import Task


{-| I wanted to make a circle drawing thing using partial application

I'd need a function that takes a Vec for the position and a radius.

I'd partially apply the Vec on mouseDown, adding the radius as a delta on mouse~move

Then adding the finnished Circle to the model on mouseUp

BUT I'm haveing some bother with the SVG because if I make it fill the whole screen than the pixel X,Y will be missmatched to the SVG scale

-}
type alias Model =
    { width : Int
    , height : Int
    , drawing : Maybe Shape
    , tool : Shape
    , shapes : List Shape
    }


type Shape
    = Circle Vec2 Vec2
    | Rectangle Vec2 Vec2


initialModel : Model
initialModel =
    { width = 1000
    , height = 800
    , drawing = Nothing
    , tool = Circle (vec2 0 0) (vec2 0 0)
    , shapes = []
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Task.perform GotViewport Browser.Dom.getViewport )


type Msg
    = NoOp
    | GotViewport Viewport
    | StartDraw Vec2
    | Draw Vec2
    | StopDraw
    | LogValue Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LogValue val ->
            let
                drawing =
                    model.drawing |> Maybe.map (always True) |> Maybe.withDefault False
            in
            if drawing then
                ( model, logPort val )

            else
                ( model, Cmd.none )

        StartDraw vec ->
            let
                draw =
                    case model.tool of
                        Rectangle _ _ ->
                            Just (Circle vec vec)

                        Circle _ _ ->
                            Just (Rectangle vec vec)

                tool =
                    Maybe.withDefault (Circle (vec2 0 0) (vec2 0 0)) draw
            in
            ( { model | drawing = draw, tool = tool }, Cmd.none )

        Draw vec ->
            let
                drawing =
                    model.drawing
                        |> Maybe.map
                            (\shape ->
                                case shape of
                                    Circle startVev _ ->
                                        Circle startVev vec

                                    Rectangle startVev _ ->
                                        Rectangle startVev vec
                             -- _ ->
                             --     shape
                            )
            in
            ( { model | drawing = drawing }, Cmd.none )

        StopDraw ->
            let
                shapes =
                    model.drawing
                        |> Maybe.map (\shape -> shape :: model.shapes)
                        |> Maybe.withDefault model.shapes
            in
            ( { model | drawing = Nothing, shapes = shapes }, Cmd.none )

        GotViewport viewport ->
            ( { model | width = viewport.viewport.width |> floor, height = viewport.viewport.height |> floor }, Cmd.none )


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
                    xEnd - xStart |> floor

                height_ =
                    yEnd - yStart |> floor
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



-- _ -> Svg.text ""


view : Model -> Html Msg
view model =
    let
        svgViewBox =
            "0 0 "
                ++ (model.width
                        |> fromInt
                   )
                ++ " "
                ++ (model.height
                        |> fromInt
                   )

        shapes =
            model.drawing
                |> Maybe.map (\shape -> shape :: model.shapes)
                |> Maybe.withDefault model.shapes
    in
    main_
        [ style "height" "100vh"
        , style "width" "100vw"
        , style "background" "pink"
        , style "display" "flex"
        , style "align-itens" "center"
        , style "justify-content" "center"
        ]
        [ svg
            [ style "height" "100%"
            , style "width" "100%"
            , viewBox svgViewBox
            ]
            (rect
                [ x "10"
                , y "10"
                , width 100
                , height 100
                , rx "5"
                , ry "5"
                ]
                []
                :: (shapes
                        |> List.map toSvg
                   )
            )
        ]


startDecoder : Json.Decode.Decoder Msg
startDecoder =
    Json.Decode.map2 vec2
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)
        |> Json.Decode.map StartDraw


drawDecoder : Json.Decode.Decoder Msg
drawDecoder =
    Json.Decode.map2 vec2
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)
        |> Json.Decode.map Draw


logDecoder : Json.Decode.Decoder Msg
logDecoder =
    Json.Decode.value |> Json.Decode.map LogValue


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown <| startDecoder
        , Browser.Events.onMouseUp <| Json.Decode.succeed StopDraw
        , Browser.Events.onMouseMove <| drawDecoder
        ]


port logPort : Value -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
