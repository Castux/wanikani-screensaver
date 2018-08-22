module Main exposing (main)

import Html
import KanjiScreen
import LoadingScreen


--(<<<) f g x y =
--    f (g x y)


(<<<) : (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(<<<) =
    (<<) << (<<)
infixr 9 <<<


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
                newState =
                    LoadingScreen.update msg model

                nextState =
                    Maybe.map2 (Kanji <<< KanjiScreen.init) newState.aspect newState.kanjis
                        |> Maybe.withDefault (Loading newState)
            in
            ( nextState, Cmd.none )

        ( KanjiMsg msg, Kanji model ) ->
            wrap Kanji KanjiMsg ( KanjiScreen.update model msg )

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
