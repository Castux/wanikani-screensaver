module Main exposing (main)

import Browser
import Html
import KanjiScreen
import LoadingScreen


type Screen
    = Loading LoadingScreen.Model
    | Kanji KanjiScreen.Model


type Msg
    = LoadingMsg LoadingScreen.Msg
    | KanjiMsg KanjiScreen.Msg


init : () -> ( Screen, Cmd Msg )
init flags =
    wrap Loading LoadingMsg LoadingScreen.init


wrap pageCon msgCon ( state, msg ) =
    ( pageCon state, Cmd.map msgCon msg )


update : Msg -> Screen -> ( Screen, Cmd Msg )
update msg screen =
    case ( msg, screen ) of
        ( LoadingMsg submsg, Loading model ) ->
            let
                updatedState =
                    LoadingScreen.update submsg model

                nextState =
                    case ( updatedState.aspect, updatedState.kanjis ) of
                        ( Just aspect, Just kanjis ) ->
                            Kanji (KanjiScreen.init aspect kanjis Nothing)

                        _ ->
                            Loading updatedState
            in
            ( nextState, Cmd.none )

        ( KanjiMsg submsg, Kanji model ) ->
            wrap Kanji KanjiMsg (KanjiScreen.update model submsg)

        _ ->
            ( screen, Cmd.none )


view : Screen -> Html.Html Msg
view screen =
    case screen of
        Loading model ->
            Html.map LoadingMsg (LoadingScreen.view model)

        Kanji model ->
            Html.map KanjiMsg (KanjiScreen.view model)


subscriptions : Screen -> Sub Msg
subscriptions screen =
    case screen of
        Loading model ->
            Sub.map LoadingMsg (LoadingScreen.subscriptions model)

        Kanji model ->
            Sub.map KanjiMsg (KanjiScreen.subscriptions model)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
