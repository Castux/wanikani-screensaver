module LoadingScreen exposing (init,update,subscriptions, Model, Msg)

import Types exposing (Kanji)
import Api
import Task
import Window
import Http

type alias Model =
    { kanjis : Maybe (List Kanji)
    , aspect : Maybe Float
    }

type Msg
    = ReceivedKanjis (Result Http.Error (List Kanji))
    | WindowResize Window.Size

initState =
    { kanjis = Nothing
    , aspect = Nothing
    }

initCommands =
    Cmd.batch
        [ Api.getData ReceivedKanjis
        , Task.perform WindowResize Window.size
        ]

init =
    (initState, initCommands)

update msg state =
    case msg of
        ReceivedKanjis (Ok kanjis) ->
            { state | kanjis = Just kanjis } ! []

        ReceivedKanjis (Err error) ->
            state ! []

        WindowResize size ->
            { state | aspect = Just (toFloat size.width/ toFloat size.height)} ! []

subscriptions state =
    Sub.batch
        [ Window.resizes WindowResize ]
