module Main exposing (..)

import Html exposing (Html, text, div, h1, input, span, ul, li)
import Html.Attributes exposing (src, class, style, value)
import Html.Events exposing (onMouseOver, onMouseOut, onInput)
import Instructions exposing (instructions)
import Kana exposing (Kana, kanas)
import Random
import Random.Array exposing (sample)
import Maybe exposing (Maybe)


---- MODEL ----


type AnswerResult
    = Correct
    | Incorrect
    | NotFinished


type alias Model =
    { currentKana : Kana -- Current kana character
    , total : Int -- Total answers
    , correct : Int -- Correct answers
    , hideAnswer : Bool -- Used for showing the answer on kana hover
    , showCorrection : Bool -- Used for showing correction on wrong input
    , wasWrong : Bool -- Was the last input wrong? Used for calculating `total`
    , input : String -- User input
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Kana "ひ" "hi") 0 0 True False False "", Random.generate NewKana (sample kanas) )



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

        Input input ->
            case checkInput input model.currentKana.answer of
                Correct ->
                    ( { model | input = "", wasWrong = False, correct = calculateCorrect model.correct model.wasWrong, total = model.total + 1, showCorrection = False }, Random.generate NewKana (sample kanas) )

                Incorrect ->
                    ( { model | wasWrong = True, input = input, showCorrection = True }, Cmd.none )

                NotFinished ->
                    ( { model | input = input, showCorrection = False }, Cmd.none )


calculateCorrect : Int -> Bool -> Int
calculateCorrect currentCorrect wasWrong =
    if wasWrong then
        currentCorrect
    else
        currentCorrect + 1


checkInput : String -> String -> AnswerResult
checkInput input answers =
    let
        answerResults =
            String.split "," answers |> List.map (\answer -> compareInput input answer)
    in
        if List.any (\x -> x == NotFinished) answerResults then
            NotFinished
        else if List.any (\x -> x == Correct) answerResults then
            Correct
        else
            Incorrect


compareInput : String -> String -> AnswerResult
compareInput input answer =
    if input == answer then
        Correct
    else if String.startsWith input answer == True then
        -- If input is partially correct, but user hasn't finished typing yet
        NotFinished
    else
        Incorrect



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
