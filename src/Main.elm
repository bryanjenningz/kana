module Main exposing (..)

import Html exposing (Html, text, div, h1, input, span, ul, li)
import Html.Attributes exposing (src, class, style)
import Html.Events exposing (onMouseOver, onMouseOut, onInput)


---- MODEL ----


type alias Kana =
    { character : String, answer : String }


type alias Model =
    { currentKana : Kana, total : Int, correct : Int, hideAnswer : Bool, showCorrection : Bool }


init : ( Model, Cmd Msg )
init =
    ( Model (Kana "ひ" "hi") 0 0 True False, Cmd.none )



---- UPDATE ----


type Msg
    = ToggleAnswer
    | Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAnswer ->
            ( { model | hideAnswer = (not model.hideAnswer) }, Cmd.none )

        Input str ->
            case str == model.currentKana.answer of
                True ->
                    ( { model | correct = model.correct + 1, total = model.total + 1 }, Cmd.none )

                False ->
                    ( { model | showCorrection = isInputCorrect str model.currentKana.answer }, Cmd.none )


isInputCorrect : String -> String -> Bool
isInputCorrect input answer =
    not (String.startsWith input answer)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "DJT Kana" ]
        , div [ class "kanabox" ]
            [ div [ class "answer", style (getAnswerStyle model.hideAnswer) ] [ text (model.currentKana.answer) ]
            , div [ class "kana" ]
                [ span [ onMouseOver ToggleAnswer, onMouseOut ToggleAnswer ] [ text (model.currentKana.character) ]
                ]
            , div [ class "input" ] [ input [ class "input-box", onInput Input ] [] ]
            , instructions model.showCorrection model.currentKana
            , ul [ class "tools" ] [ li [ class "sound" ] [], li [ class "stroke" ] [] ]
            , div [ class "counter" ] [ text (getCounterText model.correct model.total) ]
            ]
        ]


instructions : Bool -> Kana -> Html.Html msg
instructions showCorrection kana =
    case showCorrection of
        False ->
            div [ class "message" ] [ text "Hover over the kana to show its romanization and type the answer." ]

        True ->
            div [ class "message", style [ ( "color", "red" ) ] ] [ text (kana.character ++ " = " ++ kana.answer) ]


getCounterText : Int -> Int -> String
getCounterText correct total =
    case total of
        0 ->
            ""

        _ ->
            toString correct ++ "/" ++ toString total


getAnswerStyle : Bool -> List ( String, String )
getAnswerStyle hideAnswer =
    if hideAnswer then
        [ ( "visibility", "hidden" ) ]
    else
        [ ( "", "" ) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
