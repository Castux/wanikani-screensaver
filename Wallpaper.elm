module Main exposing (main)

import AnimationFrame
import Api exposing (getData)
import Array exposing (Array)
import Html
import Random
import Random.List
import Task
import Time
import Types exposing (..)
import Utils
import View
import Window


initTiles kanjis =
    kanjis
        |> List.map (Tile Nothing)


scrambleTiles ( a, b ) tiles =
    tiles
        |> Array.fromList
        |> Utils.arraySwap a b
        |> Array.toList


swapRandomPair tiles =
    let
        intGen =
            Random.int 0 (List.length tiles - 1)
    in
    Random.pair intGen intGen |> Random.generate SwapTiles


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
            state
                ! [ kanjis
                        |> Random.List.shuffle
                        |> Random.generate KanjiShuffled
                  ]

        HttpAnswer (Err error) ->
            state ! []

        WindowResize newSize ->
            { state | windowSize = newSize } ! []

        Tick dt ->
            { state | tiles = View.updateTiles dt state } ! []

        Tock dt ->
            state ! [ swapRandomPair state.tiles ]

        SwapTiles indices ->
            { state | tiles = scrambleTiles indices state.tiles } ! []

        KanjiShuffled kanjis ->
            { state | tiles = initTiles kanjis } ! []


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
