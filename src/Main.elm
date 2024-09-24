port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Html exposing (Html, button, div, main_, text, ul)
import Html.Attributes exposing (classList, height, style, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import Math.Vector2 exposing (Vec2, vec2)
import Shape exposing (Shape(..))
import String exposing (fromFloat, fromInt)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, r, rx, ry, viewBox, x, y)
import Task


type alias Model =
    { width : Int
    , height : Int
    , drawing : Maybe Shape
    , tool : Vec2 -> Shape
    , shapes : List Shape
    }


initialModel : Model
initialModel =
    { width = 1000
    , height = 800
    , drawing = Nothing
    , tool = Circle (vec2 0 0)
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
    | FinishDraw
    | LogValue Value
    | SelectTool (Vec2 -> Shape)


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

        GotViewport viewport ->
            ( { model | width = viewport.viewport.width |> floor, height = viewport.viewport.height |> floor }, Cmd.none )

        StartDraw vec ->
            let
                tool =
                    case model.tool vec of
                        Circle _ _ ->
                            Circle vec

                        Rectangle _ _ ->
                            Rectangle vec

                draw =
                    Just <| tool vec
            in
            ( { model | drawing = draw, tool = tool }, Cmd.none )

        Draw vec ->
            let
                drawing =
                    model.drawing
                        |> Maybe.map
                            (\_ ->
                                model.tool vec
                            )
            in
            ( { model | drawing = drawing }, Cmd.none )

        FinishDraw ->
            let
                shapes =
                    model.drawing
                        |> Maybe.map (\shape -> shape :: model.shapes)
                        |> Maybe.withDefault model.shapes
            in
            ( { model | drawing = Nothing, shapes = shapes }, Cmd.none )

        SelectTool tool ->
            ( { model | tool = tool }, Cmd.none )


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
        , style "position" "relative"
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
                        |> List.map Shape.toSvg
                   )
            )
        , ul
            [ style "position" "absolute"
            , style "left" "20px"
            , style "bottom" "20px"
            , style "display" "flex"
            ]
            [ Html.li []
                [ Html.button
                    [ classList
                        [ ( "text-white", True )
                        , ( "bg-gray-800", True )
                        , ( "hover:bg-gray-900", True )
                        , ( "focus:outline-none", True )
                        , ( "focus:ring-4", True )
                        , ( "focus:ring-gray-300", True )
                        , ( "font-medium", True )
                        , ( "rounded-lg", True )
                        , ( "text-sm", True )
                        , ( "px-5", True )
                        , ( "py-2.5", True )
                        , ( "me-2", True )
                        , ( "mb-2", True )
                        , ( "dark:bg-gray-800", True )
                        , ( "dark:hover:bg-gray-700", True )
                        , ( "dark:focus:ring-gray-700", True )
                        , ( "dark:border-gray-700", True )
                        ]
                    , onClick (SelectTool <| Rectangle <| vec2 0 0)
                    ]
                    [ Html.text "Rectangle" ]
                ]
            , Html.li []
                [ Html.button
                    [ classList
                        [ ( "text-white", True )
                        , ( "bg-gray-800", True )
                        , ( "hover:bg-gray-900", True )
                        , ( "focus:outline-none", True )
                        , ( "focus:ring-4", True )
                        , ( "focus:ring-gray-300", True )
                        , ( "font-medium", True )
                        , ( "rounded-lg", True )
                        , ( "text-sm", True )
                        , ( "px-5", True )
                        , ( "py-2.5", True )
                        , ( "me-2", True )
                        , ( "mb-2", True )
                        , ( "dark:bg-gray-800", True )
                        , ( "dark:hover:bg-gray-700", True )
                        , ( "dark:focus:ring-gray-700", True )
                        , ( "dark:border-gray-700", True )
                        ]
                    , onClick (SelectTool <| Circle <| vec2 0 0)
                    ]
                    [ Html.text "square" ]
                ]
            ]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown <| startDecoder
        , Browser.Events.onMouseUp <| Json.Decode.succeed FinishDraw
        , Browser.Events.onMouseMove <| drawDecoder
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


logDecoder : Json.Decode.Decoder Msg
logDecoder =
    Json.Decode.value |> Json.Decode.map LogValue


port logPort : Value -> Cmd msg
