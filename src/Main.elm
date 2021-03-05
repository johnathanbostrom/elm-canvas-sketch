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
import Time


-- ███    ███  ██████  ██████  ███████ ██
-- ████  ████ ██    ██ ██   ██ ██      ██
-- ██ ████ ██ ██    ██ ██   ██ █████   ██
-- ██  ██  ██ ██    ██ ██   ██ ██      ██
-- ██      ██  ██████  ██████  ███████ ███████


type alias Model =
    { count : Float
    , window : Window
    , highestNum : Int
    , rectangle : Rectangle
    , pressedKeys : List Keyboard.Key
    , numInput : List Int
    , score : Int
    , numScores : NumberScores
    , countDown : Int
    , currentProgress : Int
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
    { count = 0
    , window =
        { width = flag.width * 0.9
        , height = flag.height * 0.9
        }
    , highestNum = 1
    , rectangle = Rectangle 2 3
    , pressedKeys = []
    , numInput = []
    , score = 0
    , numScores = initNumberScores
    , countDown = 10
    , currentProgress = 0
    }

init : Flag -> ( Model, Cmd Msg )
init flag =
    ( initModel flag
    , Random.generate NewRectangle <| getQuestion 1 initNumberScores
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
        , Time.every 1000 Tick
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
    | Tick Time.Posix



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
            (  {model | rectangle = rectangle, countDown = 10 }, Cmd.none)
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
        Tick t ->
            if model.countDown < 2 then
                checkAnswer {model | countDown = model.countDown - 1}
            else
                ({model | countDown = model.countDown - 1}, Cmd.none)
            

        



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
        answer =
            model.numInput
            |> List.map String.fromInt
            |> String.concat
            |> String.toInt
            |> Maybe.withDefault -1

        correct = answer == model.rectangle.h * model.rectangle.w
        scores = updateNumberScores model.rectangle.h model.rectangle.w model.countDown correct model.numScores

        score = 
            if correct then 
                model.score + model.countDown
            else
                model.score - 9

        model_ =
            { model 
            | score = model.score + model.countDown
            , numInput = []
            } |> updateCurrentProgess correct

        getNextRectangle = Random.generate NewRectangle <| getQuestion model_.highestNum scores
        lg = Debug.log "prog" (model.currentProgress, model.highestNum)
    in
        ( model_, getNextRectangle)

updateCurrentProgess : Bool -> Model -> Model
updateCurrentProgess correct model =
    if not correct then
        model
    else if model.currentProgress > 5 && model.highestNum < 10 then
        { model 
        | currentProgress = 0
        , highestNum = model.highestNum + 1
        , numScores = MathGen.advanceMultiplicand (model.highestNum + 1) model.numScores
        }
    else
        { model | currentProgress = model.currentProgress + 1}

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
            ( model.window.width - 180, (model.window.height) - 45 )
            (String.padLeft 3 '0' <| input)

viewScore : Model -> List Renderable -> List Renderable
viewScore model renderables =
    renderables 
        |> textComposable [ font 20, fill colorWhilePlaying, align Left ]
            ( model.window.width - 145 , 45 )
            ((++) "SCORE:" <| String.padLeft 4 '0' <| String.fromInt model.score)

viewCountDown : Model -> List Renderable -> List Renderable
viewCountDown model renderables =
    renderables 
        |> textComposable [ font 20, fill colorWhilePlaying, align Left ]
            ( 45 , 45 )
            (String.padLeft 2 '0' <| String.fromInt model.countDown)

viewLevel : Model -> List Renderable -> List Renderable
viewLevel model renderables =
    renderables 
        |> textComposable [ font 20, fill colorWhilePlaying, align Left ]
            ( model.window.width - 260 , 45 )
            ((++) "LEVEL:" <| String.padLeft 2 '0' <| String.fromInt model.highestNum)


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
        |> viewRectangle model.rectangle model.window colorWhilePlaying
        |> viewTexts model input
        |> viewScore model
        |> viewCountDown model
        |> viewLevel model
        



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