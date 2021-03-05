module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (Setting, fill, stroke)
import Canvas.Settings.Advanced exposing (alpha)
import Canvas.Settings.Text exposing (TextAlign(..), align, font, baseLine, TextBaseLine(..))
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Html
import Html exposing (div)
import Html.Attributes exposing (style)
import MathGen exposing (randomRectangle, Rectangle, updateNumberScores, NumberScores, getQuestion, initNumberScores)
import Random
import Keyboard
import Browser.Events exposing (onKeyUp)


-- ███    ███  ██████  ██████  ███████ ██
-- ████  ████ ██    ██ ██   ██ ██      ██
-- ██ ████ ██ ██    ██ ██   ██ █████   ██
-- ██  ██  ██ ██    ██ ██   ██ ██      ██
-- ██      ██  ██████  ██████  ███████ ███████


type alias Model =
    { -- Counter
      count : Float

    -- Window
    , window : Window
    , highestNum : Int
    , rectangle : Rectangle
    , pressedKeys : List Keyboard.Key
    , numInput : List Int
    , score : Int
    , numScores : NumberScores
    }




-- ███████ ██       █████   ██████
-- ██      ██      ██   ██ ██
-- █████   ██      ███████ ██   ███
-- ██      ██      ██   ██ ██    ██
-- ██      ███████ ██   ██  ██████


type alias Flag =
    { width : Float
    , height : Float
    }



-- ██ ███    ██ ██ ████████
-- ██ ████   ██ ██    ██
-- ██ ██ ██  ██ ██    ██
-- ██ ██  ██ ██ ██    ██
-- ██ ██   ████ ██    ██


initModel : Flag -> Model
initModel flag =
    { -- Counter
      count = 0

    -- Window
    , window =
        { width = flag.width / 2
        , height = flag.height / 2
        }
    , highestNum = 3
    , rectangle = Rectangle 2 3
    , pressedKeys = []
    , numInput = []
    , score = 0
    , numScores = initNumberScores
    }

init : Flag -> ( Model, Cmd Msg )
init flag =
    ( initModel flag
    , Random.generate NewRectangle <| getQuestion 3 initNumberScores
    )



-- ███████ ██    ██ ██████  ███████  ██████ ██████  ██ ██████  ████████ ██  ██████  ███    ██ ███████
-- ██      ██    ██ ██   ██ ██      ██      ██   ██ ██ ██   ██    ██    ██ ██    ██ ████   ██ ██
-- ███████ ██    ██ ██████  ███████ ██      ██████  ██ ██████     ██    ██ ██    ██ ██ ██  ██ ███████
--      ██ ██    ██ ██   ██      ██ ██      ██   ██ ██ ██         ██    ██ ██    ██ ██  ██ ██      ██
-- ███████  ██████  ██████  ███████  ██████ ██   ██ ██ ██         ██    ██  ██████  ██   ████ ███████


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ Browser.Events.onAnimationFrameDelta Frame
        , Sub.map KeyMsg Keyboard.subscriptions
        ]



-- ███    ███ ███████  ██████
-- ████  ████ ██      ██
-- ██ ████ ██ ███████ ██   ███
-- ██  ██  ██      ██ ██    ██
-- ██      ██ ███████  ██████


type Msg
    = Frame Float
    | NewRectangle Rectangle
    | KeyMsg Keyboard.Msg



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )
        NewRectangle rectangle ->
            (  {model | rectangle = rectangle }, Cmd.none)
        KeyMsg km ->
            let
                newKeys = Keyboard.update km []
                getNum k =
                    case k of
                        Keyboard.Character c -> String.toInt c
                        _ -> Nothing
                newNums = 
                    List.filterMap getNum newKeys

                backspacePressed =
                    List.member (Keyboard.Backspace) newKeys

                enterPressed =
                    List.member (Keyboard.Enter) newKeys

                handleBackspace inputs =
                    if backspacePressed then
                        List.reverse inputs
                        |> List.drop 1 
                        |> List.reverse
                    else
                        inputs

                numInput =
                    model.numInput ++ newNums
                    |> (\l -> List.drop (List.length l - 3) l)
                    |> handleBackspace

                model_ = {model | numInput = numInput}
                
            in
            if enterPressed then
                checkAnswer model_
            else
                (model_, Cmd.none)
            
        



-- ███████ ██   ██  █████  ██████  ███████ ██████
-- ██      ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ███████ ███████ ██████  █████   ██   ██
--      ██ ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ██   ██ ██   ██ ██   ██ ███████ ██████
--
-- ████████ ██    ██ ██████  ███████ ███████
--    ██     ██  ██  ██   ██ ██      ██
--    ██      ████   ██████  █████   ███████
--    ██       ██    ██      ██           ██
--    ██       ██    ██      ███████ ███████

checkAnswer : Model -> ( Model, Cmd Msg )
checkAnswer model =
    let
        getNextRectangle = Random.generate NewRectangle <| getQuestion model.highestNum model.numScores
        answer =
            model.numInput
            |> List.map String.fromInt
            |> String.concat
            |> String.toInt
            |> Maybe.withDefault -1

        (model_, cmd) = 
            if answer == model.rectangle.h * model.rectangle.w then
                ({ model | score = model.score + 1, numInput = [] }, getNextRectangle)
            else
                ({model | numInput = []}, Cmd.none)
    in
        (model_, cmd)

type alias Window =
    { height : Float
    , width : Float
    }



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███
--
-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


colorWhilePlaying : Color.Color
colorWhilePlaying =
    Color.rgb255 139 109 154


margin : Float
margin =
    10


text : List Setting -> Canvas.Point -> String -> Renderable
text settings point string =
    Canvas.text settings
        point
        (string
            |> String.replace "a" "A"
        )


textComposable : List Setting -> Canvas.Point -> String -> List Renderable -> List Renderable
textComposable settings point string renderables =
    renderables ++ [ text settings point string ]


font : Int -> Setting
font size =
    Canvas.Settings.Text.font { size = size, family = "Major Mono Display, monospace" }



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███
--
-- ██████  ███████ ███    ██ ██████  ███████ ██████   █████  ██████  ██      ███████ ███████
-- ██   ██ ██      ████   ██ ██   ██ ██      ██   ██ ██   ██ ██   ██ ██      ██      ██
-- ██████  █████   ██ ██  ██ ██   ██ █████   ██████  ███████ ██████  ██      █████   ███████
-- ██   ██ ██      ██  ██ ██ ██   ██ ██      ██   ██ ██   ██ ██   ██ ██      ██           ██
-- ██   ██ ███████ ██   ████ ██████  ███████ ██   ██ ██   ██ ██████  ███████ ███████ ███████


viewFullscreenRect : Window -> Color.Color -> List Renderable -> List Renderable
viewFullscreenRect window color renderable =
    renderable
        ++ [ shapes [ fill color ]
                [ rect ( 0, 0 )
                    window.width
                    window.height
                ]
           ]


viewTexts : Model -> String -> List Renderable -> List Renderable
viewTexts model input renderables =
    renderables
        |> textComposable [ font 80, fill colorWhilePlaying, align Left ]
            ( model.window.width / 2 - 90, (model.window.height) - 45 )
            (String.padLeft 3 '0' <| input)


viewRectangle : Rectangle -> Window -> Color.Color -> List Renderable -> List Renderable
viewRectangle rectangle window color renderables =
    let
        basis = min (window.width/20) (window.height/20)
        width = basis * Basics.toFloat rectangle.w
        height = basis * Basics.toFloat rectangle.h
        (x, y) = (window.width/2 - width/2, window.height/2 - height/2) 
        textSettings = [ font 20, fill colorWhilePlaying, align Center, baseLine Middle ]

        textLeftPos = ( x - 20, window.height/2 )
        textRightPos = ( window.width/2, y + height + 20 )
    in
    renderables
        ++ [ shapes [ stroke color, lineWidth 2]
                [ rect (x, y) 
                    (width) 
                    (height)
                ]
           , text textSettings textLeftPos (String.fromInt rectangle.h)
           , text textSettings textRightPos (String.fromInt rectangle.w)
           ]


-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███
--
--  ██████  █████  ███    ██ ██    ██  █████  ███████
-- ██      ██   ██ ████   ██ ██    ██ ██   ██ ██
-- ██      ███████ ██ ██  ██ ██    ██ ███████ ███████
-- ██      ██   ██ ██  ██ ██  ██  ██  ██   ██      ██
--  ██████ ██   ██ ██   ████   ████   ██   ██ ███████


viewCanvas : Model -> List Renderable
viewCanvas model =
    let
        input 
            = model.numInput
            |> List.map String.fromInt
            |> String.concat
        lg = Debug.log "input " input
    in
    []
        |> viewFullscreenRect model.window (Color.rgb 0.9 0.7 1)
        |> textComposable [ font 26, align Left ] (0,0) input
        |> viewRectangle model.rectangle model.window colorWhilePlaying
        |> viewTexts model input
        



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███


view : Model -> Html.Html Msg
view model =
    div [ style "width" "100vw"
        , style "height" "100vh"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( floor model.window.width, floor model.window.height )
            []
            (viewCanvas model)
        ]



-- ███    ███  █████  ██ ███    ██
-- ████  ████ ██   ██ ██ ████   ██
-- ██ ████ ██ ███████ ██ ██ ██  ██
-- ██  ██  ██ ██   ██ ██ ██  ██ ██
-- ██      ██ ██   ██ ██ ██   ████


main : Program Flag Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }