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
  , showLetters : Bool
  , indicateRow : Bool
  }

init : (Model, Cmd Msg)
init =
  let initialState =
    { grid = generateNewGrid (List.repeat 9 0) 3
    , seekRow = 0
    , cols = 3
    , rows = 3
    , showTime = 500
    , delay = 50
    , score = 0
    , error = ""
    , showLetters = False
    , indicateRow = False
  } in
    (initialState, Cmd.none)

-- UPDATE

type Msg
  = StartRound | GetNewGrid (Int, List Int) | PromptForAnswer () | FlashGrid () | TickFail () | CheckAnswer String | SpeedUp | SpeedDown | DelayUp | DelayDown

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartRound ->
      ({ model | showLetters = False, indicateRow = False }, getNewGrid model)

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
              in
                ({model | score = model.score + rowScore, showLetters = True}, Cmd.none)
      else
        (model, Cmd.none)

    SpeedDown ->
      ({ model | showTime = Basics.max 100 (model.showTime - 25) }, Cmd.none)

    SpeedUp ->
      ({ model | showTime = Basics.min 1000 (model.showTime + 25) }, Cmd.none)

    DelayDown ->
      ({ model | delay = Basics.max 25 (model.delay - 25) }, Cmd.none)

    DelayUp ->
      ({ model | delay = Basics.min 1000 (model.delay + 25) }, Cmd.none)

generateNewGrid : List Int -> Int -> List (List Char)
generateNewGrid grid cols =
  collate cols <| List.map (\i -> Char.fromCode (65 + i)) grid

collate : Int -> List a -> List (List a)
collate n xs =
  case (List.take n xs) of
    [] -> []
    x -> x :: collate n (List.drop n xs)

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
  let
    delay = (toFloat model.showTime)
  in
    Task.perform TickFail FlashGrid <| Process.sleep delay

promptForAnswer : Model -> Cmd Msg
promptForAnswer model =
  let
    delay = (toFloat model.delay)
  in
    Task.perform TickFail PromptForAnswer <| Process.sleep delay

-- VIEW

view : Model -> Html Msg
view model =
  div
    [style
      [ ("width", "40%")
      , ("height", "75%")
      , ("text-align", "center")
      , ("margin", "auto")
      , ("margin-top", "10%")
      ]
    ]

    [ showGrid model
    , br [] []
    , input [ placeholder "Letters...", onInput CheckAnswer ] []
    , br [] []
    , text (toString model.score)
    , button [ onClick StartRound ] [ text "Start" ]
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
