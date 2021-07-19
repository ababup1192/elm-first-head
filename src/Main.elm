module Main exposing (Msg(..), eatBento, findMaxNum, update)

import Browser
import Html exposing (Html, button, div, li, span, text, ul)
import Html.Events exposing (onClick)


type alias Bento =
    { dish : String, num : Int }


type alias Model =
    { bentoList : List Bento, eatCount : Int }


initialModel : Model
initialModel =
    let
        karaben1 =
            { dish = "唐揚げ", num = 10 }

        karaben2 =
            { dish = "唐揚げ", num = 8 }

        karaben3 =
            { dish = "唐揚げ", num = 6 }
    in
    { bentoList = [ karaben1, karaben2, karaben3 ], eatCount = 0 }


type Msg
    = Eat


findMaxNum : List Bento -> Int
findMaxNum bentoList =
    bentoList |> List.map (\bento -> bento.num) |> List.maximum |> Maybe.withDefault 0


eatBento : List Bento -> Int -> List Bento
eatBento bentoList maximumNum =
    case bentoList of
        [] ->
            []

        bento :: rest ->
            if bento.num == maximumNum then
                { bento | num = bento.num - 1 } :: rest

            else
                bento :: eatBento rest maximumNum


update : Msg -> Model -> Model
update msg model =
    case msg of
        Eat ->
            let
                maximumNum =
                    findMaxNum model.bentoList
            in
            if maximumNum == 0 then
                model

            else
                { model | bentoList = eatBento model.bentoList maximumNum, eatCount = model.eatCount + 1 }


isEmptyBentoList : List Bento -> Bool
isEmptyBentoList bentoList =
    List.all (\bento -> bento.num == 0) bentoList


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Eat ] [ text "食べる" ]
        , span []
            [ text <|
                "  "
                    ++ String.fromInt model.eatCount
                    ++ "個食べた。"
                    ++ (if isEmptyBentoList model.bentoList then
                            "もう食べれない。"

                        else
                            ""
                       )
            ]
        , ul [] <|
            List.map
                (\bento ->
                    li [] [ text <| bento.dish ++ String.fromInt bento.num ++ "個" ]
                )
                model.bentoList
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
