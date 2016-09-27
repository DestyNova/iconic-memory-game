import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Char
import Process
import Random
import String
import Task
import Json.Decode as Json

main =
  App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- MODEL

type alias Model =
  { grid : List (List Char)
  , seekRow : Int
  , cols : Int
  , rows : Int
  , showTime : Int
  , delay : Int
  , score : Int
  , error : String
  , letters : String
  , showLetters : Bool
  , indicateRow : Bool
  }

init : (Model, Cmd Msg)
init =
  let initialState =
    { grid = generateNewGrid (List.repeat 12 0) 4
    , seekRow = 0
    , cols = 4
    , rows = 3
    , showTime = 450
    , delay = 50
    , score = 0
    , error = ""
    , letters = ""
    , showLetters = False
    , indicateRow = False
  } in
    (initialState, getNewGrid initialState)

-- UPDATE

type Msg
  = StartNextRound () | StartRound () | GetNewGrid (Int, List Int) | PromptForAnswer () | FlashGrid () | TickFail () |
    CheckAnswer String | SpeedUp | SpeedDown | DelayUp | DelayDown | ColsUp | ColsDown | RowsUp | RowsDown |
    SkipOnEnter Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartNextRound _ ->
      startRound model

    StartRound _ ->
      (model, getNewGrid model)

    GetNewGrid random ->
      ({ model | showLetters = True, seekRow = (fst random), grid = generateNewGrid (snd random) model.cols }, flashGrid model)

    FlashGrid _ ->
      ({ model | showLetters = False }, promptForAnswer model)

    PromptForAnswer _ ->
      ({ model | indicateRow = True }, Cmd.none)

    TickFail _ ->
      (model, Cmd.none)

    CheckAnswer letters ->
      if String.length letters == model.cols then
        let
          thisRow = List.head <| List.drop model.seekRow model.grid
        in
          case thisRow of
            Nothing ->
              ({model | error = "Grid is missing a row!"}, Cmd.none)

            Just row ->
              let
                numCorrect = sum <|
                  map2 (\a b -> if a==b then 1 else 0)
                                   (String.toList <| String.toUpper letters)
                                   row
                rowScore = numCorrect * 100 // model.cols
                model' = {model | score = model.score + rowScore, letters = "", showLetters = True}
              in
                (model', startNextRound model')
      else
        ({model | letters = letters}, Cmd.none)

    SpeedDown ->
      ({ model | showTime = Basics.max 100 (model.showTime - 25) }, Cmd.none)

    SpeedUp ->
      ({ model | showTime = Basics.min 1000 (model.showTime + 25) }, Cmd.none)

    DelayDown ->
      ({ model | delay = Basics.max 25 (model.delay - 25) }, Cmd.none)

    DelayUp ->
      ({ model | delay = Basics.min 1000 (model.delay + 25) }, Cmd.none)

    ColsDown ->
      ({ model | cols = Basics.max 3 (model.cols - 1) }, Cmd.none)

    ColsUp ->
      ({ model | cols = Basics.min 6 (model.cols + 1) }, Cmd.none)

    RowsDown ->
      ({ model | rows = Basics.max 3 (model.rows - 1) }, Cmd.none)

    RowsUp ->
      ({ model | rows = Basics.min 6 (model.rows + 1) }, Cmd.none)

    SkipOnEnter keycode ->
      case keycode of
        13 -> startRound model
        _ -> (model, Cmd.none)


generateNewGrid : List Int -> Int -> List (List Char)
generateNewGrid grid cols =
  collate cols <| List.map (\i -> Char.fromCode (65 + i)) grid

collate : Int -> List a -> List (List a)
collate n xs =
  case (List.take n xs) of
    [] -> []
    x -> x :: collate n (List.drop n xs)

startRound model =
  ({ model | letters = "", showLetters = False, indicateRow = False }, waitToStartRound model)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- TASKS

getNewGrid : Model -> Cmd Msg
getNewGrid model =
  Random.generate GetNewGrid <| Random.pair (Random.int 0 (model.rows - 1)) (Random.list (model.cols * model.rows) (Random.int 0 25))

flashGrid : Model -> Cmd Msg
flashGrid model =
  Task.perform TickFail FlashGrid <| Process.sleep (toFloat model.showTime)

promptForAnswer : Model -> Cmd Msg
promptForAnswer model =
  Task.perform TickFail PromptForAnswer <| Process.sleep (toFloat model.delay)

startNextRound : Model -> Cmd Msg
startNextRound model =
  Task.perform TickFail StartNextRound <| Process.sleep (toFloat 1000)

waitToStartRound : Model -> Cmd Msg
waitToStartRound model =
  Task.perform TickFail StartRound <| Process.sleep (toFloat 500)

-- VIEW

view : Model -> Html Msg
view model =
  div
    [style
      [ ("width", "20%")
      , ("height", "75%")
      , ("text-align", "center")
      , ("margin", "auto")
      , ("margin-top", "10%")
      ]
    ]

    [ showGrid model
    , br [] []
    , input [ placeholder "Letters, or enter to skip", value model.letters, onInput CheckAnswer, onKeyDown SkipOnEnter ] []
    , br [] []
    , text <| String.append "Score: " (toString model.score)
    , br [] []
    , text "Show time (ms):"
    , button [ onClick SpeedDown ] [ text "-" ]
    , text (toString model.showTime)
    , button [ onClick SpeedUp ] [ text "+" ]
    , br [] []
    , text "Prompt delay (ms):"
    , button [ onClick DelayDown ] [ text "-" ]
    , text (toString model.delay)
    , button [ onClick DelayUp ] [ text "+" ]
    , br [] []
    , text "Columns:"
    , button [ onClick ColsDown ] [ text "-" ]
    , text (toString model.cols)
    , button [ onClick ColsUp ] [ text "+" ]
    , br [] []
    , text "Rows:"
    , button [ onClick RowsDown ] [ text "-" ]
    , text (toString model.rows)
    , button [ onClick RowsUp ] [ text "+" ]
    , br [] []
    , a [href "http://github.com/DestyNova/iconic-memory-game"] [text "Source"]
    ]

showGrid : Model -> Html Msg
showGrid model =
  let
    rowStyle =
      [ style
        [ ("justify-content", "space-around")
        , ("align-items", "stretch")
        , ("display", "flex")
        , ("font-size", "24pt")
        ]
      ]

    tileStyle =
      [ style
        [ ("color", "navy")
        , ("backgroundColor", "cornsilk")
        , ("flex", "1 1 50%")
        , ("height", "50%")
        ]
      ]
  in
    div [] <|
      List.concatMap
        (\indexedRow -> [div rowStyle <|
          List.concat
            [ [ getMarker (model.indicateRow && model.seekRow == fst indexedRow) ">" ]
            , List.concatMap (\letter ->
                [div tileStyle [text <| if model.showLetters then String.fromList [letter] else "_"]])
              (snd indexedRow)
            , [ getMarker (model.indicateRow && model.seekRow == fst indexedRow) "<" ]]
          ])
        (List.indexedMap (,) model.grid)

getMarker showMarker symbol =
  let
    markerStyle =
      [ style
        [ ("color", "red")
        , ("opacity", if showMarker then "1" else "0.1")
        ]
      ]
  in
    div markerStyle [ text symbol ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)
