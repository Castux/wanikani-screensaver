module KanjiScreen exposing (Model, Msg, init, subscriptions, update, view)

import Html
import Html.Attributes
import KanjiData exposing (KanjiData)
import Layout
import Palettes
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Window


type Msg
    = WindowResize Window.Size


type alias Model =
    { aspect : Float
    , kanjis : List KanjiData
    }


init =
    Model


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
            1


update model msg =
    case msg of
        WindowResize size ->
            { model | aspect = toFloat size.width / toFloat size.height } ! []


viewKanjis kanjis aspect =
    let
        ( tiles, ( w, h ) ) =
            kanjis
                |> List.map (\kd -> ( kd, sizing kd.srs ))
                |> (\l -> Layout.computeLayout l aspect (Random.initialSeed 0))

        tw =
            toString <| 32 * w

        th =
            toString <| 32 * h
    in
    tiles
        |> List.map viewKanji
        |> g
            [ fontFamily "Noto Sans CJK JP"
            , fontSize "32px"
            , dominantBaseline "hanging"

            --            , textAnchor "middle"
            ]
        |> List.singleton
        |> svg
            [ viewBox <| "0 0 " ++ tw ++ " " ++ th
            , Html.Attributes.style [ ( "margin", "auto" ), ( "width", "100%" ), ( "height", "100%" ) ]
            ]


kanjiColor k =
    case k.srs of
        Nothing ->
            "#202020"

        Just level ->
            Palettes.wanikani level


viewKanji : ( KanjiData, Int, ( Int, Int ) ) -> Svg msg
viewKanji ( data, size, ( x, y ) ) =
    let
        trans =
            "translate("
                ++ toString (x * 32)
                ++ " "
                ++ toString (y * 32)
                ++ ")"
                ++ " scale("
                ++ toString size
                ++ ") "
    in
    text_
        [ fill (kanjiColor data)
        , transform trans
        ]
        [ text data.character ]


view state =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "min-height", "100%" )
            , ( "background", "black" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ viewKanjis state.kanjis state.aspect ]


subscriptions state =
    Sub.batch
        [ Window.resizes WindowResize ]
