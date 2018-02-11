module Instructions exposing (instructions)

import Html exposing (Html, text, div, h1, input, span, ul, li)
import Html.Attributes exposing (src, class, style, value)
import Kana exposing (Kana)


instructions : Bool -> Kana -> Html.Html msg
instructions showCorrection kana =
    case showCorrection of
        False ->
            div [ class "message" ] [ text "Hover over the kana to show its romanization and type the answer." ]

        True ->
            div [ class "message", style [ ( "color", "red" ) ] ] [ text (kana.character ++ " = " ++ getFirstAnswer kana.answer) ]


getFirstAnswer : String -> String
getFirstAnswer answers =
    -- Show only the first answer of all possilbe ones
    case String.split "," answers |> List.head of
        Just answer ->
            answer

        Nothing ->
            ""
