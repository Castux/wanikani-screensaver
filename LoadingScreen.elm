module LoadingScreen exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Html
import Html.Attributes
import Http
import KanjiData exposing (KanjiData)
import Task
import Window


type alias Model =
    { kanjis : Maybe (List KanjiData)
    , aspect : Maybe Float
    }


type Msg
    = ReceivedKanjis (Result Http.Error (List KanjiData))
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
    ( initState, initCommands )


update : Msg -> Model -> Model
update msg state =
    case msg of
        ReceivedKanjis (Ok kanjis) ->
            { state | kanjis = Just kanjis }

        ReceivedKanjis (Err error) ->
            state

        WindowResize size ->
            { state | aspect = Just (toFloat size.width / toFloat size.height) }


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
        [ Html.div
            [ Html.Attributes.style
                [ ( "color", "white" )
                , ( "font-size", "20vmin" )
                , ( "margin", "auto" )
                ]
            ]
            [ Html.text "ロード中" ]
        ]


subscriptions state =
    Sub.batch
        [ Window.resizes WindowResize ]
