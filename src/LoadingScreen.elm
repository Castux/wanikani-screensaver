module LoadingScreen exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Dom
import Browser.Events
import Html
import Html.Attributes exposing (style)
import Http
import KanjiData exposing (KanjiData)
import Task


type alias Model =
    { kanjis : Maybe (List KanjiData)
    , aspect : Maybe Float
    , errorMsg : Maybe String
    }


type Msg
    = ReceivedKanjis (Result Http.Error (List KanjiData))
    | WindowResize Float Float


initState : Model
initState =
    { kanjis = Nothing
    , aspect = Nothing
    , errorMsg = Nothing
    }


viewportToSize : Browser.Dom.Viewport -> Msg
viewportToSize vp =
    WindowResize
        vp.viewport.width
        vp.viewport.height


initCommands : Cmd Msg
initCommands =
    Cmd.batch
        [ Api.getData ReceivedKanjis
        , Task.perform viewportToSize Browser.Dom.getViewport
        ]


init : Maybe String -> ( Model, Cmd Msg )
init maybeKey =
    ( initState, initCommands )


update : Msg -> Model -> Model
update msg state =
    case msg of
        ReceivedKanjis (Ok kanjis) ->
            { state | kanjis = Just kanjis }

        ReceivedKanjis (Err error) ->
            state

        WindowResize width height ->
            { state | aspect = Just (width / height) }


view : Model -> Html.Html Msg
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


subscriptions : Model -> Sub Msg
subscriptions state =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h)) ]
