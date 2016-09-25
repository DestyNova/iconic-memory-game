import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import String

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
  , delay : Int
  , score : Int
  , error : String
  }

init : (Model, Cmd Msg)
init =
  let initialState =
    { grid = [['a', 'b', 'c', 'd'], ['d', 'e', 'f', 'g']]
    , seekRow = 0
    , cols = 4
    , rows = 4
    , delay = 50
    , score = 0
    , error = ""
  } in
    (initialState, Cmd.none)

-- UPDATE

type Msg
  = CheckAnswer String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
                                   (String.toList letters)
                                   row
                rowScore = numCorrect * 100 // model.cols
              in
                ({model | score = model.score + rowScore}, Cmd.none)
      else
        (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- TASKS
-- ?

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    showGrid model
  , input [ placeholder "Letters...", onInput CheckAnswer ] []
  , br [] []
  , text (toString model.score)
  ]

showGrid : Model -> Html Msg
showGrid model =
    div [] <| List.concatMap (\row -> [div [] <| List.concatMap (\letter -> [text <| String.fromList [letter]]) row, br [] []]) model.grid

