module Main exposing (..)
import Browser
import Html exposing (Html, div, img, text, input)
import Html.Attributes exposing (style, src, value)
import Html.Events
import String exposing (String)
import Svg exposing (svg, line)
import Svg.Attributes exposing (viewBox, x1, y1, x2, y2, stroke, strokeWidth, width, height)
import Svg.Attributes exposing (strokeOpacity)
import Html exposing (button)
import Html.Events exposing (onClick)
import Svg.Attributes exposing (opacity)

type alias Model =
    { sliderXValue : Float
    , eyeRotation : Float
    , ghostOpacity : String
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
    , ghostOpacity = "0"
    , imageSource = "img/face_eyes_open.png"
    , ghostX = 200.0
    , ghostY = 500.0
    , ghostWidth = 40.0
    , ghostHeight = 40.0
    , collision = False
    , line2X1 = 400.0
    , line2Y1 = 300.0
    , line2X2 = 400.0
    , line2Y2 = 600.0
    , line3X1 = 400.0
    , line3Y1 = 600.0
    , line3X2 = 400.0
    , line3Y2 = 300.0
    }

-- Function to check if two line segments intersect
lineIntersect : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
lineIntersect (lx1, ly1) (lx2, ly2) (lx3, ly3) (lx4, ly4) =
    let
        denominator = (ly4 - ly3) * (lx2 - lx1) - (lx4 - lx3) * (ly2 - ly1)
        ua = ((lx4 - lx3) * (ly1 - ly3) - (ly4 - ly3) * (lx1 - lx3)) / denominator
        ub = ((lx2 - lx1) * (ly1 - ly3) - (ly2 - ly1) * (lx1 - lx3)) / denominator
    in
    denominator /= 0 && ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1

-- Function to check if a line intersects with a rectangle
isLineIntersectRect : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
isLineIntersectRect x1 y1 x2 y2 rx ry rw rh =
    let
        -- Rectangle corners
        topLeft = (rx, ry)
        topRight = (rx + rw, ry)
        bottomLeft = (rx, ry + rh)
        bottomRight = (rx + rw, ry + rh)

        -- Check if the line intersects any of the rectangle's sides
        intersectsTop = lineIntersect (x1, y1) (x2, y2) topLeft topRight
        intersectsBottom = lineIntersect (x1, y1) (x2, y2) bottomLeft bottomRight
        intersectsLeft = lineIntersect (x1, y1) (x2, y2) topLeft bottomLeft
        intersectsRight = lineIntersect (x1, y1) (x2, y2) topRight bottomRight
    in
    intersectsTop || intersectsBottom || intersectsLeft || intersectsRight

type Msg
    = SliderChange String
    | TurnOnGhost

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
                
                updatedModel =
                    { model | sliderXValue = newValue, eyeRotation = newRotation }
        
                line2X2 =
                    ((100 - updatedModel.sliderXValue) / 100) * 800 + (updatedModel.eyeRotation * 10)
        
                line3X1 =
                    ((100 - updatedModel.sliderXValue) / 100) * 800 + (updatedModel.eyeRotation * 10)
        
                line3X2 =
                    ((100 - updatedModel.sliderXValue) / 100) * 800 + (updatedModel.eyeRotation * 25)
        
                collisionWithLine2 =
                    isLineIntersectRect updatedModel.line2X1 updatedModel.line2Y1 line2X2 updatedModel.line2Y2 model.ghostX model.ghostY model.ghostWidth model.ghostHeight
                collisionWithLine3 = 
                    isLineIntersectRect line3X1 updatedModel.line3Y1 line3X2 updatedModel.line3Y2 model.ghostX model.ghostY model.ghostWidth model.ghostHeight
                collision = collisionWithLine2 || collisionWithLine3
            in
            { updatedModel | line2X2 = line2X2, line3X1 = line3X1, line3X2 = line3X2, collision = collision }

        TurnOnGhost ->
            if model.ghostOpacity == "1" then 
            { model | ghostOpacity = "0" }
            else
            { model | ghostOpacity = "1"}

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
    div [ 
        ]
        [ 
            -- container for the simulation
             div [ style "width" "800px"
            , style "height" "900px"
            , style "position" "relative"
            , style "margin" "0 auto"
            , style "border" "1px solid black"
        ]
        [ 
            -- reality room
            div [ style "position" "absolute"
            , style "top" "300px"
            , style "width" "100%"
            , style "height" "300px"
            , style "background-color" "#ffffff"
            ]
            [ input [ Html.Attributes.type_ "range"
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
                    ]
        , div [ style "position" "absolute"
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
                    , y1 "450px"
                    , x2 "400px"
                    , y2 "300px"
                    , stroke "#f094c5"
                    , strokeWidth "5"
                    , strokeOpacity "0.5"
                    ]
                    []
                    , line
                    [ x1 (String.fromFloat model.line2X1 ++ "px")
                    , y1 (String.fromFloat model.line2Y1 ++ "px")
                    , x2 (String.fromFloat model.line2X2 ++ "px")
                    , y2 (String.fromFloat model.line2Y2 ++ "px")
                    , stroke "#f094c5"
                    , strokeWidth "5"
                    , strokeOpacity "0.5"
                    ] []
                    , line
                    [ x1 (String.fromFloat model.line3X1 ++ "px")
                    , y1 (String.fromFloat model.line3Y1 ++ "px")
                    , x2 (String.fromFloat model.line3X2 ++ "px")
                    , y2 (String.fromFloat model.line3Y2 ++ "px")
                    , stroke "#f094c5"
                    , strokeWidth "5"
                    , strokeOpacity "0.5"
                    ] []
                ]
                -- face rotates when dragged from side to side
            ], img [ src "img/dragUI.png"
                , style "position" "absolute"
                , style "top" "50%"
                , style "left" (String.fromFloat model.sliderXValue ++ "%")
                , style "transform" ("translate(-50%, -50%)")
                , style "transform-origin" "50% 50%"
                ,style "pointer-events" "none"
                ] []  
            , img [ src "img/face_eyes_open.png"
                , style "position" "absolute"
                , style "top" "50%"
                , style "left" (String.fromFloat model.sliderXValue ++ "%")
                , style "transform" ("translate(-50%, -50%) rotate(" ++ String.fromFloat (model.eyeRotation*1.5) ++ "deg)")
                , style "transform-origin" "50% 50%"
                ,style "pointer-events" "none"
                ] []    
        -- top virtual room
        , div [ 
            style "position" "absolute"
            , style "top" "0"
            , style "width" "100%"
            , style "height" "300px"
            , style "background-color" "#F6FAFF"
        ][ img [ 
            -- mirror image of ghost
            src "img/ghost.png"
            , style "position" "absolute"
            , style "left" (String.fromFloat model.ghostX ++ "px")
            , style "top" (String.fromFloat (100-(((model.ghostY - 300)/300)*100)) ++ "%")
            , style "transform" "translate(-50%, -50%) scaleY(-1)"
            , style "transform-origin" "50% 50%"
            , style "opacity" "0.5"
            ] []
            ]
        -- bottom virtual room
        , div [ 
            style "position" "absolute"
            , style "top" "600px"
            , style "width" "100%"
            , style "height" "300px"
            , style "background-color" "#F6FAFF"
        ][ img [ 
            -- mirror image of ghost
            src "img/ghost.png"
            , style "position" "absolute"
            , style "left" (String.fromFloat model.ghostX ++ "px")
            , style "top" (String.fromFloat (100-(((model.ghostY - 300)/300)*100)) ++ "%")
            , style "transform" "translate(-50%, -50%) scaleY(-1)"
            , style "transform-origin" "50% 50%"
            , style "opacity" "0.5"
            ] []
            ]
        , makeAMirror "295px"
        , makeAMirror "595px"
        -- the ghost in the room
        , img [ 
            src "img/ghost.png"
            , style "position" "absolute"
            , style "left" (String.fromFloat model.ghostX ++ "px")
            , style "top" (String.fromFloat model.ghostY ++ "px")
            , style "transform" "translate(-50%, -50%)"
            , style "transform-origin" "50% 50%"
            , style "pointer-events" "none"
            , style "opacity" model.ghostOpacity
            ] []
        ]
        -- under simulation
        , div [ style "position" "relative"
        , style "top" "20px"
        , style "left" "50%"
        , style "transform" "translateX(-50%)"
        , style "text-align" "center"
        , style "font-family" "Arial"
        ]
        [ text "Use the positions of the ghost's reflections in the virtual rooms to determine the ghost in the real room's position. Adjust the person to look at the real ghost." ]
        , div [ style "position" "absolute"
                , style "top" "950px"
                , style "left" "50%"
                , style "transform" "translateX(-50%)"
        ]
        [ button [ onClick TurnOnGhost ] [ text "Check your answer" ]
        ]
    ]
-- if model.collision then "Collision detected!" else "No collision"
main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }