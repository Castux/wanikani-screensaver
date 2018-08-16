module LoadingScreen exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Html
import Html.Attributes
import Http
import Task
import Types exposing (Kanji)
import Window


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
    ( initState, initCommands )


fullyLoaded state =
    Maybe.map2 (,) state.kanjis state.aspect


update : Msg -> Model -> ( Model, Maybe ( List Kanji, Float ) )
update msg state =
    let
        updatedState =
            case msg of
                ReceivedKanjis (Ok kanjis) ->
                    { state | kanjis = Just kanjis }

                ReceivedKanjis (Err error) ->
                    state

                WindowResize size ->
                    { state | aspect = Just (toFloat size.width / toFloat size.height) }
    in
    ( updatedState, fullyLoaded updatedState )


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
        [ Html.span
            [ Html.Attributes.style
                [ ( "color", "white" )
                , ( "font-size", "20vmin" )
                , ( "font-family", "Noto Sans CJK JP" )
                , ( "margin", "auto" )
                ]
            ]
            [ Html.text "ロード中" ]
        ]


subscriptions state =
    Sub.batch
        [ Window.resizes WindowResize ]
