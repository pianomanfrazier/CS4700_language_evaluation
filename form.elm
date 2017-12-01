import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char exposing (isDigit, isUpper)

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update}

-- MODEL

type alias Model = 
    { name          : String,
      age           : Int,
      password      : String,
      passwordAgain : String,
      clicked       : Bool
    }

model : Model
model = 
    Model "" -1 "" "" False 

-- UPDATE

type Msg =
    Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Clicked

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        Name name ->
            { model | name = name }
        Age age ->
            {model | age = Result.withDefault -1 (String.toInt age)}
        Password password ->
            { model | password = password }
        PasswordAgain password ->
            { model | passwordAgain = password }
        Clicked ->
            { model | clicked = True }

-- VIEW

inputClass : Html.Attribute msg
inputClass = 
    class "b mv3 ph3 pv2 input-reset ba b--black bg-black-70 white grow point db"
submitClass : Html.Attribute msg 
submitClass = 
    class "b b--blue mv3 ph3 pv2 input-reset ba bg-blue grow pointer db"

view : Model -> Html Msg 
view model = 
    div [class ""]
    [
    Html.h1 [class "tc h1 pb5"] [text "Password validator"]
    , div [class "ba br3 b--black-50 pa2 measure center bg-black-10"] [
        input [inputClass, type_ "text", placeholder "Name", onInput Name] [],
        input [inputClass, type_ "text", placeholder "Age", onInput Age] [],
        input [inputClass, type_ "password", placeholder "Password", onInput Password] [],
        input [inputClass, type_ "password", placeholder "Re-enter Password", onInput PasswordAgain] [],
        input [submitClass, type_ "submit", value "Check password", onClick Clicked] [],
        viewValidation model
      ]
    ]

passValid : String -> Bool
passValid str = 
    let
        len    = String.length str >= 8

        digit  = String.any isDigit str 

        upper  = String.any isUpper str
    in
        len && digit && upper


viewValidation : Model -> Html msg 
viewValidation model = 
    let
        (color, message) = 
        if model.password /= model.passwordAgain then
            ("yellow", "Passwords do not match")
        else if not (passValid model.password) then
            ("red", "Password must include uppercase, digits, and > 8" )
        else if (model.age < 1) || (model.age > 150) then
            ("pink", "Invalid age" )
        else
            ("green", "Valid Password")
    in 
        if model.clicked then
            div [class ("ba br3 b--" ++ color ++ " pa3 bg-light-" ++ color)] [div [] [text message]]
        else
            div [] []
