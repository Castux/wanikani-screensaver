module Main exposing (main)

import AnimationFrame
import Api exposing (getData)
import Html
import Task
import Types exposing (..)
import View
import Window
import Time
import Random
import Array


initTiles kanjis =
    kanjis
        |> List.map (Tile Nothing)


scrambleTiles (a,b) tiles =
    let
        array = Array.fromList tiles

        tilesToSwap =
            Maybe.map2 (,)
                    (Array.get a array)
                    (Array.get b array)
    in
        case tilesToSwap of
            Just (ta, tb) ->
                array
                    |> Array.set b ta
                    |> Array.set a tb
                    |> Array.toList

            Nothing ->
                tiles

randomPairGenerator numTiles =
    let
        intGen = Random.int 0 (numTiles - 1)
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


update : Message -> State -> (State, Cmd Message)
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
