module LoadingScreen exposing (Model, Msg, Params, init, subscriptions, update, view)

import Api
import Browser.Dom
import Browser.Events
import Html
import Html.Attributes exposing (style)
import Http
import KanjiData exposing (KanjiData)
import Task


type alias Params =
    { key : String
    , padding : Maybe Int
    }


type alias Model =
    { kanjis : Maybe (List KanjiData)
    , aspect : Maybe Float
    , errorMsg : Maybe String
    , padding : Int
    }


type Msg
    = ReceivedKanjis (Result Http.Error (List KanjiData))
    | WindowResize Float Float


initState : Params -> Model
initState params =
    { kanjis = Nothing
    , aspect = Nothing
    , errorMsg = Nothing
    , padding = params.padding |> Maybe.withDefault 0
    }


viewportToSize : Browser.Dom.Viewport -> Msg
viewportToSize vp =
    WindowResize
        vp.viewport.width
        vp.viewport.height


initCommands : Params -> Cmd Msg
initCommands params =
    Cmd.batch
        [ Api.getData ReceivedKanjis params.key
        , Task.perform viewportToSize Browser.Dom.getViewport
        ]


errorState : String -> Model
errorState msg =
    { kanjis = Nothing
    , aspect = Nothing
    , errorMsg = Just msg
    , padding = 0
    }


init : Maybe Params -> ( Model, Cmd Msg )
init maybeParams =
    case maybeParams of
        Just params ->
            ( initState params, initCommands params )

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
            { state | aspect = Just ((width - 2 * toFloat state.padding) / (height - 2 * toFloat state.padding)) }


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
