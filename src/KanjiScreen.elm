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
    | ShuffleTime Posix
    | Tick Posix


type alias Tile =
    ( KanjiData, Int, ( Int, Int ) )


type alias Model =
    { aspect : Float
    , tiles : List Tile
    , gridSize : ( Int, Int )
    , time : Float
    }


init : Float -> List KanjiData -> Model
init aspect kanjis =
    let
        ( tiles, gridSize ) =
            kanjis
                |> List.map (\kd -> ( kd, sizing kd.srs ))
                |> (\l -> Layout.computeLayout l aspect (Random.initialSeed 0))
    in
    Model aspect tiles gridSize 0.0


shuffle : Model -> Random.Seed -> Model
shuffle model seed =
    let
        ( shuffled, gridSize ) =
            model.tiles
                |> List.map (\( data, size, _ ) -> ( data, size ))
                |> (\l -> Layout.computeLayout l model.aspect seed)
    in
    { model | tiles = shuffled, gridSize = gridSize }


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
            ( init (width / height) kanjis, Cmd.none )

        ShuffleTime time ->
            let
                seed =
                    Random.initialSeed (Time.posixToMillis time)
            in
            ( shuffle model seed, Cmd.none )

        Tick t ->
            ( { model | time = Time.posixToMillis t |> toFloat }, Cmd.none )


viewKanjis : List Tile -> ( Int, Int ) -> Float -> Svg Msg
viewKanjis tiles ( w, h ) time =
    let
        tw =
            String.fromInt <| referenceScale * w

        th =
            String.fromInt <| referenceScale * h
    in
    tiles
        |> List.map (viewKanji time)
        |> Svg.g [ fontSize <| String.fromInt referenceScale ++ "px" ]
        |> List.singleton
        |> Svg.svg
            [ viewBox <| "0 0 " ++ tw ++ " " ++ th
            , style "margin:auto; width: 100%; height: 100%;"
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


viewKanji : Float -> Tile -> Svg Msg
viewKanji time ( data, size, ( x, y ) ) =
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
        [ fill (kanjiColor data time ( x, y ))
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
        [ viewKanjis state.tiles state.gridSize state.time ]


subscriptions : Model -> Sub Msg
subscriptions state =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        , Browser.Events.onAnimationFrame Tick
        , Time.every (15.0 * 1000) ShuffleTime
        ]
