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


initCommands : String -> Cmd Msg
initCommands key =
    Cmd.batch
        [ Api.getData ReceivedKanjis key
        , Task.perform viewportToSize Browser.Dom.getViewport
        ]


errorState : String -> Model
errorState msg =
    { kanjis = Nothing
    , aspect = Nothing
    , errorMsg = Just msg
    }


init : Maybe String -> ( Model, Cmd Msg )
init maybeKey =
    case maybeKey of
        Just key ->
            ( initState, initCommands key )

        Nothing ->
            ( errorState "(屮｀∀´)屮", Cmd.none )


update : Msg -> Model -> Model
update msg state =
    case msg of
        ReceivedKanjis (Ok kanjis) ->
            { state | kanjis = Just kanjis }

        ReceivedKanjis (Err error) ->
            errorState "(◕︿◕✿)"

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
            , style "text-align" "center"
            , style "margin" "auto"
            ]
            [ Html.text <| Maybe.withDefault "ロード中" state.errorMsg ]
        ]


subscriptions : Model -> Sub Msg
subscriptions state =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h)) ]
