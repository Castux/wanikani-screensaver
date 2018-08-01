module Main exposing (main)

import Api exposing (getData)
import Html
import Task
import Types exposing (..)
import View
import Window


init =
    let
        initState =
            { windowSize = Window.Size 800 600
            , kanjis = []
            }

        initCommands =
            Cmd.batch
                [ getData
                , Task.perform WindowResize Window.size
                ]
    in
    ( initState, initCommands )


update : Message -> State -> State
update msg state =
    case msg of
        Nop ->
            state

        HttpAnswer (Ok kanjis) ->
            { state | kanjis = kanjis }

        HttpAnswer (Err error) ->
            state

        WindowResize newSize ->
            { state | windowSize = newSize }


subscriptions state =
    Window.resizes WindowResize


main =
    Html.program
        { init = init
        , view = View.view
        , update = \msg state -> ( update msg state, Cmd.none )
        , subscriptions = subscriptions
        }
