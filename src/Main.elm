port module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Html exposing (Html, button, div, main_, text)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import String exposing (fromInt)
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
    = Circle Int Int Int
    | Rectangle Int Int Int Int


initialModel : Model
initialModel =
    { width = 1000
    , height = 800
    , drawing = Nothing
    , tool = Circle 0 0 0
    , shapes = []
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Task.perform GotViewport Browser.Dom.getViewport )


type Msg
    = NoOp
    | GotViewport Viewport
    | StartDraw Int Int
    | Draw Int Int
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

        StartDraw x y ->
            let
                draw =
                    case model.tool of
                        Rectangle _ _ _ _ ->
                            Just (Circle x y 0)

                        Circle _ _ _ ->
                            Just (Rectangle x y x y)

                tool =
                    case model.tool of
                        Rectangle _ _ _ _ ->
                            Circle x y 0

                        Circle _ _ _ ->
                            Rectangle x y 0 0
            in
            ( { model | drawing = draw, tool = tool }, Cmd.none )

        Draw x y ->
            let
                drawing =
                    model.drawing
                        |> Maybe.map
                            (\shape ->
                                case shape of
                                    Circle x_ y_ _ ->
                                        let
                                            xDelta =
                                                x - x_

                                            yDelta =
                                                y - y_

                                            delta =
                                                ((xDelta ^ 2) + (yDelta ^ 2))
                                                    |> toFloat
                                                    |> sqrt
                                                    |> floor
                                        in
                                        Circle x_ y_ delta

                                    Rectangle x_ y_ _ _ ->
                                        Rectangle x_ y_ x y
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
        Circle x y radius ->
            let
                x_ =
                    fromInt x

                y_ =
                    fromInt y

                r_ =
                    fromInt radius
            in
            circle
                [ cx x_
                , cy y_
                , r r_
                ]
                []

        Rectangle x__ y__ x_ y_ ->
            let
                xStart =
                    min x__ x_

                yStart =
                    min y__ y_

                xEnd =
                    max x__ x_

                yEnd =
                    max y__ y_

                width_ =
                    xEnd - xStart

                height_ =
                    yEnd - yStart
            in
            rect
                [ x <| fromInt xStart
                , y <| fromInt yStart
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
    Json.Decode.map2 StartDraw
        (Json.Decode.field "pageX" Json.Decode.float |> Json.Decode.map truncate)
        (Json.Decode.field "pageY" Json.Decode.float |> Json.Decode.map truncate)


drawDecoder : Json.Decode.Decoder Msg
drawDecoder =
    Json.Decode.map2 Draw
        (Json.Decode.field "pageX" Json.Decode.float |> Json.Decode.map truncate)
        (Json.Decode.field "pageY" Json.Decode.float |> Json.Decode.map truncate)


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
