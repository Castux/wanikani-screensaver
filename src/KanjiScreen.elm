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
import Time exposing (Posix)


type Msg
    = WindowResize Float Float
    | ShuffleTime Posix


type alias Tile =
    ( KanjiData, Int, ( Int, Int ) )


type alias Model =
    { aspect : Float
    , tiles : List Tile
    , gridSize : ( Int, Int )
    }


init : Float -> List KanjiData -> Maybe Random.Seed -> Model
init aspect kanjis maybeSeed =
    let
        seed =
            Maybe.withDefault (Random.initialSeed 0) maybeSeed

        ( tiles, gridSize ) =
            kanjis
                |> List.map (\kd -> ( kd, sizing kd.srs ))
                |> (\l -> Layout.computeLayout l aspect seed)
    in
    Model aspect tiles gridSize


referenceScale : Int
referenceScale =
    100


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
    let
        kanjis =
            List.map (\( a, b, c ) -> a) model.tiles
    in
    case msg of
        WindowResize width height ->
            ( init (width / height) kanjis Nothing, Cmd.none )

        ShuffleTime time ->
            let
                seed =
                    Just <| Random.initialSeed (Time.posixToMillis time)
            in
            ( init model.aspect kanjis seed, Cmd.none )


viewKanjis : List Tile -> ( Int, Int ) -> Svg Msg
viewKanjis tiles ( w, h ) =
    let
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


viewKanji : Tile -> Svg Msg
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
        [ viewKanjis state.tiles state.gridSize ]


subscriptions : Model -> Sub Msg
subscriptions state =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        , Time.every (10.0 * 1000) ShuffleTime
        ]
