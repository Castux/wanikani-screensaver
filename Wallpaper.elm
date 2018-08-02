module Main exposing (main)

import AnimationFrame
import Api exposing (getData)
import Array
import Html
import Random
import Task
import Time
import Types exposing (..)
import View
import Window


initTiles kanjis =
    kanjis
        |> List.sortBy .level
        |> List.map (Tile Nothing)


scrambleTiles ( a, b ) tiles =
    let
        array =
            Array.fromList tiles
    in
    Maybe.map2
        (\ta tb ->
            array
                |> Array.set b ta
                |> Array.set a tb
                |> Array.toList
        )
        (Array.get a array)
        (Array.get b array)
        |> Maybe.withDefault tiles


randomPairGenerator numTiles =
    let
        intGen =
            Random.int 0 (numTiles - 1)
    in
    Random.pair intGen intGen |> Random.generate Shuffle


init : ( State, Cmd Message )
init =
    let
        initState =
            { windowSize = Window.Size 800 600
            , tiles = []
            }

        initCommands =
            Cmd.batch
                [ getData
                , Task.perform WindowResize Window.size
                ]
    in
    ( initState, initCommands )


update : Message -> State -> ( State, Cmd Message )
update msg state =
    case msg of
        Nop ->
            state ! []

        HttpAnswer (Ok kanjis) ->
            { state | tiles = initTiles kanjis } ! []

        HttpAnswer (Err error) ->
            state ! []

        WindowResize newSize ->
            { state | windowSize = newSize } ! []

        Tick dt ->
            { state | tiles = View.updateTiles dt state } ! []

        Tock dt ->
            state ! [ randomPairGenerator (List.length state.tiles) ]

        Shuffle indices ->
            { state | tiles = scrambleTiles indices state.tiles } ! []


subscriptions state =
    Sub.batch
        [ Window.resizes WindowResize
        , AnimationFrame.diffs Tick
        , Time.every (0.1 * Time.second) Tock
        ]


main =
    Html.program
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }
