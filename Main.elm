module Main exposing (main)

import LoadingScreen
import Html

type Screen
    = Loading LoadingScreen.Model

type Msg
    = LoadingMsg LoadingScreen.Msg

init =
    wrap Loading LoadingMsg LoadingScreen.init

wrap pageCon msgCon (state, msg) =
    ( pageCon state, Cmd.map msgCon msg )

update : Msg -> Screen -> (Screen, Cmd Msg)
update msg screen =
    case (msg, screen) of
        (LoadingMsg msg, Loading model) ->
            wrap Loading LoadingMsg (LoadingScreen.update msg model)
        


main =
    Html.program
        { init = init
        , update = update
        , view = \state -> Html.div [] []
        , subscriptions = \state -> Sub.none
        }
