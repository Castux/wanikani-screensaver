module KanjiScreen exposing (Model, Msg, init, subscriptions, update, view)

import Anim exposing (grayscale, plasma, toCss)
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
    | Tick Float


type alias Tile =
    ( KanjiData, Int, ( Int, Int ) )


type alias Model =
    { aspect : Float
    , padding : Int
    , kanjis : List KanjiData
    , tiles : List Tile
    , gridSize : ( Int, Int )
    , time : Float
    , iterTime : Float
    }


shufflePeriod =
    24.0


fadeInTime =
    2.1


kanjiOrder ka kb =
    compare
        (ka.srs |> Maybe.withDefault 100)
        (kb.srs |> Maybe.withDefault 100)


init : Float -> Int -> List KanjiData -> Model
init aspect padding kanjis =
    let
        ( tiles, gridSize ) =
            kanjis
                |> List.sortWith kanjiOrder
                |> List.map (\kd -> ( kd, sizing kd.srs ))
                |> (\l -> Layout.computeLayout l aspect (Random.initialSeed 0))
    in
    Model aspect padding kanjis tiles gridSize 0.0 0.0


shuffle : Model -> Float -> Model
shuffle model time =
    let
        seed =
            Random.initialSeed (floor time)

        ( shuffled, gridSize ) =
            model.kanjis
                |> List.sortWith kanjiOrder
                |> List.map (\kd -> ( kd, sizing kd.srs ))
                |> (\l -> Layout.computeLayout l model.aspect seed)
    in
    { model | tiles = shuffled, gridSize = gridSize, time = time, iterTime = 0 }


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
            1


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        WindowResize width height ->
            ( init
                ((width - 2 * toFloat model.padding) / (height - 2 * toFloat model.padding))
                model.padding
                (List.map (\( a, b, c ) -> a) model.tiles)
            , Cmd.none
            )

        Tick t ->
            let
                newTime =
                    model.time + t / 1000

                iterTime =
                    model.iterTime + t / 1000

                updated =
                    if iterTime > shufflePeriod then
                        shuffle model newTime

                    else
                        { model | time = newTime, iterTime = iterTime }
            in
            ( updated, Cmd.none )


viewKanjis : List Tile -> ( Int, Int ) -> Float -> Float -> Int -> Svg Msg
viewKanjis tiles ( w, h ) time iterTime padding =
    let
        tw =
            String.fromInt <| referenceScale * w

        th =
            String.fromInt <| referenceScale * h
    in
    tiles
        |> List.indexedMap (viewKanji time iterTime (List.length tiles))
        |> Svg.g [ fontSize <| String.fromInt referenceScale ++ "px" ]
        |> List.singleton
        |> Svg.svg
            [ viewBox <| "0 0 " ++ tw ++ " " ++ th
            , style <|
                "margin:auto; width: calc(100% - "
                    ++ String.fromInt (2 * padding)
                    ++ "px); height: calc(100% - "
                    ++ String.fromInt (2 * padding)
                    ++ "px);"
            ]


kanjiColor : KanjiData -> Float -> ( Int, Int ) -> String
kanjiColor k time ( x, y ) =
    case k.srs of
        Nothing ->
            Anim.plasma (toFloat x) (toFloat y) time
                |> (\u -> 0.15 + u * 0.05)
                |> Anim.grayscale
                |> Anim.toCss

        Just level ->
            0.93 |> Anim.grayscale |> Anim.toCss


viewKanji : Float -> Float -> Int -> Int -> Tile -> Svg Msg
viewKanji time iterTime numTiles index ( data, size, ( x, y ) ) =
    let
        fade =
            toFloat index * fadeInTime / toFloat numTiles

        leech =
            data.correct |> Maybe.map ((>=) 0.75) |> Maybe.withDefault False

        leechRot =
            if leech then
                Anim.vibrate (time + toFloat index / 100)

            else
                0.0

        show =
            iterTime > 1.0 + fade && iterTime < shufflePeriod - fadeInTime - 1.0 + fade

        disp =
            if show then
                transform <|
                    "translate("
                        ++ String.fromInt (x * referenceScale)
                        ++ " "
                        ++ String.fromInt (y * referenceScale)
                        ++ ")"
                        ++ " scale("
                        ++ String.fromInt size
                        ++ ")"
                        ++ " rotate("
                        ++ String.fromFloat leechRot
                        ++ ","
                        ++ String.fromFloat (toFloat referenceScale / 2.0)
                        ++ ","
                        ++ String.fromFloat (toFloat referenceScale / 2.0)
                        ++ ")"

            else
                display "none"
    in
    Svg.text_
        [ fill (kanjiColor data time ( x, y ))
        , disp
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
        [ viewKanjis state.tiles state.gridSize state.time state.iterTime state.padding ]


subscriptions : Model -> Sub Msg
subscriptions state =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        , Browser.Events.onAnimationFrameDelta Tick
        ]
