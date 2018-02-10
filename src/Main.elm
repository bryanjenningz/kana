module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, text, div, h1, input, span, ul, li)
import Html.Attributes exposing (src, class, style, value)
import Html.Events exposing (onMouseOver, onMouseOut, onInput)
import Random
import Random.Array exposing (sample)
import Maybe exposing (Maybe)


---- MODEL ----


type alias Kana =
    { character : String, answer : String }


kanas : Array Kana
kanas =
    Array.fromList [ (Kana "ひ" "hi"), (Kana "あ" "a") ]


type alias Model =
    { currentKana : Kana
    , total : Int
    , correct : Int
    , hideAnswer : Bool
    , showCorrection : Bool
    , input : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Kana "ひ" "hi") 0 0 True False "", Cmd.none )



---- UPDATE ----


type Msg
    = ToggleAnswer
    | Input String
    | NewKana (Maybe Kana)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewKana maybeKana ->
            case maybeKana of
                Nothing ->
                    ( model, Cmd.none )

                Just kana ->
                    ( { model | currentKana = kana }, Cmd.none )

        ToggleAnswer ->
            ( { model | hideAnswer = (not model.hideAnswer) }, Cmd.none )

        Input str ->
            case str == model.currentKana.answer of
                True ->
                    ( { model | input = "", correct = model.correct + 1, total = model.total + 1 }, Random.generate NewKana (sample kanas) )

                False ->
                    ( { model | input = str, showCorrection = (isInputCorrect str model.currentKana.answer) }, Cmd.none )


isInputCorrect : String -> String -> Bool
isInputCorrect input answer =
    not (String.startsWith input answer)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
            , div [ class "input" ] [ input [ class "input-box", onInput Input, value model.input ] [] ]
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
        , subscriptions = subscriptions
        }
