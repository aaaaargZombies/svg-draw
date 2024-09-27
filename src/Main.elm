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
import TW.Html
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
    | Reset
    | Print


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

                        Square _ _ ->
                            Square vec

                        Polygon _ _ ->
                            case model.drawing of
                                Just (Polygon vecs _) ->
                                    Polygon <| vec :: vecs

                                _ ->
                                    Polygon [ vec ]

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
                drawing =
                    model.drawing
                        |> Maybe.andThen
                            (\shape ->
                                case shape of
                                    Polygon vecs last ->
                                        let
                                            closed =
                                                vecs
                                                    |> List.reverse
                                                    |> List.head
                                                    |> Maybe.map (\start -> Math.Vector2.distance start last < 20)
                                                    |> Maybe.withDefault False
                                        in
                                        if closed then
                                            Nothing

                                        else
                                            -- not working...
                                            Just <| Polygon (last :: vecs) last

                                    _ ->
                                        Nothing
                            )

                shapes =
                    case drawing of
                        Nothing ->
                            model.drawing
                                |> Maybe.map (\shape -> shape :: model.shapes)
                                |> Maybe.withDefault model.shapes

                        Just _ ->
                            model.shapes
            in
            ( { model | drawing = drawing, shapes = shapes }, Cmd.none )

        SelectTool tool ->
            ( { model | tool = tool }, Cmd.none )

        Undo ->
            let
                shapes =
                    model.shapes |> List.tail |> Maybe.withDefault []
            in
            ( { model | drawing = Nothing, shapes = shapes }, Cmd.none )

        Reset ->
            ( { model | drawing = Nothing, shapes = [] }, Cmd.none )

        Print ->
            ( model, printPort () )


viewSvg : Model -> Html msg
viewSvg model =
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
    Svg.svg
        [ Svg.Attributes.id "SVG"
        , Svg.Attributes.viewBox svgViewBox

        -- , Html.Attributes.style "background" "pink"
        , Svg.Attributes.class "bg-pink-200"
        ]
        (shapes
            |> List.map Shape.toSvg
        )


view : Model -> Html Msg
view model =
    Html.main_
        [ Html.Attributes.classList
            [ ( "flex", True )
            , ( "h-screen", True )
            , ( "w-screen", True )

            -- , ( "bg-pink-200", True )
            ]
        ]
        [ viewSvg model
        , Html.ul
            [ Html.Attributes.classList
                [ ( "flex", True )
                , ( "absolute", True )
                , ( "bottom-5", True )
                , ( "left-5", True )
                ]
            ]
            [ Html.li []
                [ TW.Html.button
                    [ onClick (SelectTool <| Rectangle <| vec2 0 0)
                    ]
                    [ Html.text "Rectangle" ]
                ]
            , Html.li []
                [ TW.Html.button
                    [ onClick (SelectTool <| Square <| vec2 0 0)
                    ]
                    [ Html.text "Square" ]
                ]
            , Html.li []
                [ TW.Html.button
                    [ onClick (SelectTool <| Circle <| vec2 0 0)
                    ]
                    [ Html.text "Circle" ]
                ]
            , Html.li []
                [ TW.Html.button
                    [ onClick (SelectTool <| Polygon [])
                    ]
                    [ Html.text "Polygon" ]
                ]
            , Html.li []
                [ TW.Html.button
                    [ onClick Print
                    ]
                    [ Html.text "Print" ]
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

                    "R" ->
                        Reset

                    "r" ->
                        SelectTool <| Rectangle <| vec2 0 0

                    "c" ->
                        SelectTool <| Circle <| vec2 0 0

                    "s" ->
                        SelectTool <| Square <| vec2 0 0

                    "p" ->
                        SelectTool <| Polygon []

                    "P" ->
                        Print

                    _ ->
                        NoOp
            )


subscriptions : Model -> Sub Msg
subscriptions { drawing } =
    Sub.batch <|
        [ Browser.Events.onMouseDown <| startDecoder
        , Browser.Events.onMouseUp <| Json.Decode.succeed FinishDraw
        , Browser.Events.onKeyDown <| keyDecoder
        ]
            ++ (drawing
                    |> Maybe.map (always [ Browser.Events.onMouseMove <| drawDecoder ])
                    |> Maybe.withDefault []
               )


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


port printPort : () -> Cmd msg
