module Main exposing (main)

import Browser
import Html
import KanjiScreen
import LoadingScreen



--(<<<) f g x y =
--    f (g x y)


apply2 : (a -> b) -> (c -> d -> a) -> (c -> d -> b)
apply2 =
    (<<) << (<<)


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
                newState =
                    LoadingScreen.update submsg model

                nextState =
                    Maybe.map2 (apply2 Kanji KanjiScreen.init) newState.aspect newState.kanjis
                        |> Maybe.withDefault (Loading newState)
            in
            ( nextState, Cmd.none )

<<<<<<< HEAD:Main.elm
        ( KanjiMsg msg, Kanji model ) ->
            wrap Kanji KanjiMsg (KanjiScreen.update model msg)
=======
        ( KanjiMsg submsg, Kanji model ) ->
            wrap Kanji KanjiMsg (KanjiScreen.update model submsg)
>>>>>>> b5222a95d4f5d0cf7acafd76e9e072647e474658:src/Main.elm

        _ ->
            ( screen, Cmd.none )


view : Screen -> Html.Html Msg
view screen =
    case screen of
        Loading model ->
            Html.map LoadingMsg (LoadingScreen.view model)

        Kanji model ->
            Html.map KanjiMsg (KanjiScreen.view model)


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