module LoadingScreen exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Events
import Html
import Html.Attributes exposing (style)
import Http
import KanjiData exposing (KanjiData)
import Task


type alias Model =
    { kanjis : Maybe (List KanjiData)
    , aspect : Maybe Float
    }


type Msg
    = ReceivedKanjis (Result Http.Error (List KanjiData))
    | WindowResize Int Int


initState =
    { kanjis = Nothing
    , aspect = Nothing
    }


initCommands =
    Cmd.batch
        [ Api.getData ReceivedKanjis
        ]


init =
    ( initState, initCommands )


update : Msg -> Model -> Model
update msg state =
    case msg of
        ReceivedKanjis (Ok kanjis) ->
            { state | kanjis = Just kanjis }

        ReceivedKanjis (Err error) ->
            state

        WindowResize width height ->
            { state | aspect = Just (toFloat width / toFloat height) }


view state =
    Html.div
        [ style "display" "flex"
        , style "min-height" "100%"
        , style "background" "black"
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ Html.div
            [ style "color" "white"
            , style "font-size" "20vmin"
            , style "margin" "auto"
            ]
            [ Html.text "ロード中" ]
        ]


subscriptions state =
    Sub.batch
        [ Browser.Events.onResize WindowResize ]
