port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import Math.Vector2 exposing (Vec2, vec2)
import Shape exposing (Shape(..))
import String
import Svg
import Svg.Attributes
import Task


type alias Model =
    { width : Float
    , height : Float
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
    | Undo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LogValue val ->
            ( model, logPort val )

        GotViewport viewport ->
            ( { model | width = viewport.viewport.width, height = viewport.viewport.height }, Cmd.none )

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

        Undo ->
            let
                shapes =
                    model.shapes |> List.tail |> Maybe.withDefault []
            in
            ( { model | drawing = Nothing, shapes = shapes }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        svgViewBox =
            "0 0 "
                ++ (model.width
                        |> String.fromFloat
                   )
                ++ " "
                ++ (model.height
                        |> String.fromFloat
                   )

        shapes =
            model.drawing
                |> Maybe.map (\shape -> shape :: model.shapes)
                |> Maybe.withDefault model.shapes
    in
    Html.main_
        [ Html.Attributes.classList
            [ ( "flex", True )
            , ( "h-screen", True )
            , ( "w-screen", True )
            , ( "bg-pink-200", True )
            ]
        ]
        [ Svg.svg
            [ Svg.Attributes.class "h-screen"
            , Svg.Attributes.class "w-screen"
            , Svg.Attributes.viewBox svgViewBox
            ]
            (shapes
                |> List.map Shape.toSvg
            )
        , Html.ul
            [ Html.Attributes.classList
                [ ( "flex", True )
                , ( "absolute", True )
                , ( "bottom-5", True )
                , ( "left-5", True )
                ]
            ]
            [ Html.li []
                [ Html.button
                    [ Html.Attributes.classList
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
                    [ Html.Attributes.classList
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
                    [ Html.text "Circle" ]
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


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map
            (\key ->
                case key of
                    "z" ->
                        Undo

                    "r" ->
                        SelectTool <| Rectangle <| vec2 0 0

                    "c" ->
                        SelectTool <| Circle <| vec2 0 0

                    _ ->
                        NoOp
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onMouseDown <| startDecoder
        , Browser.Events.onMouseUp <| Json.Decode.succeed FinishDraw
        , Browser.Events.onMouseMove <| drawDecoder
        , Browser.Events.onKeyDown <| keyDecoder
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
