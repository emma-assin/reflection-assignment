module Main exposing (..)
import Browser
import Html exposing (Html, div, img, text, input)
import Html.Attributes exposing (style, src, value)
import Html.Events
import String exposing (String)
import Svg exposing (svg, line)
import Svg.Attributes exposing (viewBox, x1, y1, x2, y2, stroke, strokeWidth, strokeLinecap, width, height)
import Svg.Attributes exposing (strokeOpacity)
import Browser.Dom exposing (Element)
import Debug exposing (toString)
import Html exposing (button)
import Html.Events exposing (onClick)
import Svg.Attributes exposing (x)
import Svg exposing (rect)
import Svg.Attributes exposing (y)
import Svg exposing (circle)
import Svg.Attributes exposing (cx)
import Svg.Attributes exposing (cy)
import Svg.Attributes exposing (r)
import Svg.Attributes exposing (fill)
import Svg exposing (polygon)
import Svg.Attributes exposing (points)
import Svg.Attributes exposing (opacity)

type alias Model =
    { sliderXValue : Float
    , eyeRotation : Float
    , lineOpacity : String
    , imageSource : String
    , ghostX : Float
    , ghostY : Float
    , ghostWidth : Float
    , ghostHeight : Float
    , collision : Bool
    , line2X1 : Float
    , line2Y1 : Float
    , line2X2 : Float
    , line2Y2 : Float
    , line3X1 : Float
    , line3Y1 : Float
    , line3X2 : Float
    , line3Y2 : Float
   }

init : Model
init =
    { sliderXValue = 50.0
    , eyeRotation = 0.0
    , lineOpacity = "0"
    , imageSource = "img/face_eyes_open.png"
    , ghostX = 150.0
    , ghostY = 1000.0
    , ghostWidth = 50.0
    , ghostHeight = 50.0
    , collision = False
    , line2X1 = 0.0
    , line2Y1 = 0.0
    , line2X2 = 0.0
    , line2Y2 = 0.0
    , line3X1 = 0.0
    , line3Y1 = 0.0
    , line3X2 = 0.0
    , line3Y2 = 0.0
    }

-- check for collision between a line and a rectangle
isLineIntersectRect : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
isLineIntersectRect x1 y1 x2 y2 rx ry rw rh =
    let
        -- Check if the line intersects any of the rectangle's sides
        intersectsTop = (y1 - y2) * (rx - x1) + (x2 - x1) * (ry - y1) == 0
        intersectsBottom = (y1 - y2) * (rx - x1) + (x2 - x1) * (ry + rh - y1) == 0
        intersectsLeft = (y1 - y2) * (rx - x1) + (x2 - x1) * (ry - y1) == 0
        intersectsRight = (y1 - y2) * (rx + rw - x1) + (x2 - x1) * (ry - y1) == 0
    in
    intersectsTop || intersectsBottom || intersectsLeft || intersectsRight


type Msg
    = SliderChange String
    | HideLines

update : Msg -> Model -> Model
update msg model =
    case msg of
        SliderChange value ->
            let
                newValue =
                    case String.toFloat value of
                        Just v -> v
                        Nothing -> model.sliderXValue

                newRotation =
                    if newValue < 50 then
                        (50 - newValue)
                    else
                        -(newValue - 50)

                lineX1 = 0
                lineY1 = 100
                lineX2 = ((100 - newValue) / 100) * 1205 + (newRotation * 35)
                lineY2 = 0

                collision = isLineIntersectRect lineX1 lineY1 lineX2 lineY2 model.ghostX model.ghostY model.ghostWidth model.ghostHeight
            in
            { model | sliderXValue = newValue, eyeRotation = newRotation, collision = collision }

        HideLines ->
            if model.lineOpacity == "0.5" then 
            { model | lineOpacity = "0" }
            else
            { model | lineOpacity = "0.5"}

makeAMirror : String -> Html msg
makeAMirror yPos =
    div [ style "position" "absolute"
          , style "top" yPos
          , style "left" "0"
          , style "width" "100%"
          , style "height" "10px"
          , style "background-color" "#269dc5"
          ]
        []

view : Model -> Html Msg
view model =
    div [ style "width" "100%"
        , style "height" "100%"
        ]
        [ div [ style "position" "relative"
        , style "top" "0"
        , style "left" "50%"
        , style "height" "100px"
        , style "transform" "translateX(-50%)"
        , style "text-align" "center"
        , style "font-family" "Arial"
        ]
        [ text "Mirror Simulation" ]
            -- container for the simulation
            ,div [ style "width" "1205px"
            , style "height" "800px"
            , style "top" "100"
            , style "position" "relative"
            , style "margin" "0 auto"
            , style "border" "1px solid black"
            , style "background-color" "#F6FAFF"
        ]
        -- room
        [   input [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "100"
                    , Html.Attributes.value (String.fromFloat model.sliderXValue)
                    , Html.Events.onInput SliderChange
                    , style "position" "absolute"
                    , style "top" "40%"
                    , style "left" "10px"
                    , style "width" "calc(100% - 20px)"
                    , style "height" "100px"
                    , style "cursor" "pointer"
                    , style "opacity" "0"
                    ] []
        , makeAMirror "0px"
        , makeAMirror "calc(100% - 10px)"
        , div [ style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "pointer-events" "none"
            ]
            [ svg 
            -- to visualise the paths the light takes
                [ viewBox "0 0 100% 100%"
                , width "100%"
                , height "100%"
                ]
                [ line 
                    [ x1 (String.fromFloat (model.sliderXValue) ++ "%")
                    , y1 "50%"
                    , x2 "50%"
                    , y2 "0px"
                    , stroke "#f094c5"
                    , strokeWidth "5"
                    , strokeLinecap "round"
                    , strokeOpacity model.lineOpacity
                    ]
                    []
                    , line
                    [ x1 "50%"
                    , y1 "0px"
                    , x2 (String.fromFloat (((100-model.sliderXValue)/100) * 1205 + (model.eyeRotation*10)) ++ "px")
                    , y2 "100%"
                    , stroke "#f094c5"
                    , strokeWidth "5"
                    , strokeLinecap "round"
                    , strokeOpacity model.lineOpacity
                    ] []
                    , line
                    [ x1 (String.fromFloat (((100-model.sliderXValue)/100) * 1205 + (model.eyeRotation*10)) ++ "px")
                    , y1 "100%"
                    , x2 (String.fromFloat (((100-model.sliderXValue)/100) * 1205 + (model.eyeRotation*35)) ++ "px")
                    , y2 "0px"
                    , stroke "#f094c5"
                    , strokeWidth "5"
                    , strokeLinecap "round"
                    , strokeOpacity model.lineOpacity
                    ] []
                ]
            ], img [ src model.imageSource
                , style "position" "absolute"
                , style "top" "50%"
                , style "left" (String.fromFloat model.sliderXValue ++ "%")
                , style "transform" ("translate(-50%, -50%) rotate(" ++ String.fromFloat model.eyeRotation ++ "deg)")
                , style "transform-origin" "50% 50%"
                ,style "pointer-events" "none"
                ] []
                , img [ src "img/ghost.png"
                , style "position" "absolute"
                , style "top" "150px"
                , style "left" "1000px"
                , style "transform" "translate(-50%, -50%)"
                , style "transform-origin" "50% 50%"
                ,style "pointer-events" "none"
                ] []
        ]
        , div [ style "position" "relative"
        , style "top" "20px"
        , style "left" "50%"
        , style "transform" "translateX(-50%)"
        , style "text-align" "center"
        , style "font-family" "Arial"
        ]
        [ text (if model.collision then "Collision detected!" else "No collision") ]
        , div [ style "position" "absolute"
                , style "top" "950px"
                , style "left" "50%"
                , style "transform" "translateX(-50%)"
        ]
        [ button [ onClick HideLines ] [ text "Toggle the lines on and off" ]
        ]
    ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }