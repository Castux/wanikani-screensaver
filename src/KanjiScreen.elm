module KanjiScreen exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Html exposing (Html)
import Html.Attributes
import KanjiData exposing (KanjiData)
import Layout
import Palettes
import Random
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type Msg
    = WindowResize Int Int


type alias Model =
    { aspect : Float
    , kanjis : List KanjiData
    }


referenceScale : Int
referenceScale =
    100


init : Float -> List KanjiData -> Model
init =
    Model


sizing : Maybe Int -> Int
sizing srs =
    case srs of
        Just 1 ->
            5

        Just 2 ->
            4

        Just 3 ->
            3

        Just 4 ->
            3

        Just 5 ->
            2

        Just 6 ->
            2

        Just 7 ->
            1

        Just 8 ->
            1

        _ ->
            0


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        WindowResize width height ->
            ( { model | aspect = toFloat width / toFloat height }, Cmd.none )


viewKanjis : List KanjiData -> Float -> Svg ()
viewKanjis kanjis aspect =
    let
        ( tiles, ( w, h ) ) =
            kanjis
                |> List.map (\kd -> ( kd, sizing kd.srs ))
                |> (\l -> Layout.computeLayout l aspect (Random.initialSeed 0))

        tw =
            String.fromInt <| referenceScale * w

        th =
            String.fromInt <| referenceScale * h
    in
    tiles
        |> List.map viewKanji
        |> Svg.g [ fontSize <| String.fromInt referenceScale ++ "px" ]
        |> List.singleton
        |> Svg.svg
            [ viewBox <| "0 0 " ++ tw ++ " " ++ th
            , style "margin:auto; width: 100%; height: 100%;"
            ]


kanjiColor : KanjiData -> String
kanjiColor k =
    case k.srs of
        Nothing ->
            "#202020"

        Just level ->
            "rgb(238, 238, 236)"


viewKanji : ( KanjiData, Int, ( Int, Int ) ) -> Svg Msg
viewKanji ( data, size, ( x, y ) ) =
    let
        trans =
            "translate("
                ++ String.fromInt (x * referenceScale)
                ++ " "
                ++ String.fromInt (y * referenceScale)
                ++ ")"
                ++ " scale("
                ++ String.fromInt size
                ++ ") "
    in
    Svg.text_
        [ fill (kanjiColor data)
        , transform trans
        , dy "0.875em"
        ]
        [ Svg.text data.character ]


view : Model -> Html Msg
view state =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "min-height" "100%"
        , Html.Attributes.style "background" "black"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        ]
        [ viewKanjis state.kanjis state.aspect ]


subscriptions state =
    Sub.batch
        [ Browser.Events.onResize WindowResize ]
