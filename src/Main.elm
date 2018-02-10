module Main exposing (..)

import Html exposing (Html, text, div, h1, input, span)
import Html.Attributes exposing (src, class, style)
import Html.Events exposing (onMouseOver, onMouseOut)


---- MODEL ----


type alias Kana =
    { character : String, answer : String }


type alias Model =
    { currentKana : Kana, total : Int, correct : Int, hideAnswer : Bool }


init : ( Model, Cmd Msg )
init =
    ( Model (Kana "ひ" "hi") 0 0 True, Cmd.none )



---- UPDATE ----


type Msg
    = ToggleAnswer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAnswer ->
            ( { model | hideAnswer = (not model.hideAnswer) }, Cmd.none )



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
            , div [ class "input" ] [ input [ class "input-box" ] [] ]
            , div [ class "message" ] [ text getMessage ]
            ]
        ]


getMessage : String
getMessage =
    "Hover over the kana to show its romanization and type the answer."


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
