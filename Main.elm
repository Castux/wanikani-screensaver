module Main exposing (main)

import Html
import KanjiScreen
import LoadingScreen


type Screen
    = Loading LoadingScreen.Model
    | Kanji KanjiScreen.Model


type Msg
    = LoadingMsg LoadingScreen.Msg
    | KanjiMsg KanjiScreen.Msg


init =
    wrap Loading LoadingMsg LoadingScreen.init


wrap pageCon msgCon ( state, msg ) =
    ( pageCon state, Cmd.map msgCon msg )


update : Msg -> Screen -> ( Screen, Cmd Msg )
update msg screen =
    case ( msg, screen ) of
        ( LoadingMsg msg, Loading model ) ->
            let
                ( newState, fullyLoaded ) =
                    LoadingScreen.update msg model
            in
            case fullyLoaded of
                Nothing ->
                    ( Loading newState, Cmd.none )

                Just ( kanjis, ratio ) ->
                    ( Kanji <| KanjiScreen.init kanjis ratio, Cmd.none )

        _ ->
            ( screen, Cmd.none )


view : Screen -> Html.Html Msg
view screen =
    case screen of
        Loading model ->
            LoadingScreen.view model

        Kanji model ->
            KanjiScreen.view model


subscriptions screen =
    case screen of
        Loading model ->
            Sub.map LoadingMsg (LoadingScreen.subscriptions model)

        Kanji model ->
            Sub.map KanjiMsg (KanjiScreen.subscriptions model)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
