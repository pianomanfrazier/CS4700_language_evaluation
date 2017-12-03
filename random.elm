import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import List


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
-- MODEL

-- make a list of die each with own number

type alias Model =
  { dieFaces  : (List Int)
  , numFaces  : Int
  }

init : (Model, Cmd Msg)
init =
  (Model (List.repeat 8 0) 8, Cmd.none)

-- UPDATE
type Msg
  = Roll
  | NewFaces (List Int) 
  | NumDice String

dieGenerator : Int -> Random.Generator (List Int)
dieGenerator n = 
  Random.list n (Random.int 0 5)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFaces (dieGenerator model.numFaces))

    NewFaces faces ->
      ({ model | dieFaces = faces }, Cmd.none)

    NumDice n ->
      ({ model | numFaces = 
        let
          num = Result.withDefault 1 (String.toInt n)
        in
          if num > 0 then num else 1}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

fRolling : String
fRolling = "dice_rolling.gif"

fDice    : String
fDice    = "dice.png"

dSize   : Int
dSize   = 50

dice : Int -> Html msg
dice num = div [ class "ma1 dib",
           style [ ("background-image", "url(" ++ fDice ++ ")")
                 , ("background-size", "cover")
                 , ("background-position", toString (num * -50) ++ "px 0px")
                 , ("width", toString dSize ++ "px")
                 , ("height", toString dSize ++  "px") ]
          ] [] 


view : Model -> Html Msg
view model =
  div [ class "center tc mt4 measure"]
    [ button [ onClick Roll ] [ text "Roll" ]
    , input [type_ "text", placeholder "Number of Dice", onInput NumDice] []
    , div [] (List.map dice model.dieFaces)
    ]
