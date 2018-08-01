module Api exposing (getData)

import Http
import Json.Decode exposing (..)
import Types exposing (..)


kanjiDecode =
    map6 Kanji
        (field "level" int)
        (field "character" string)
        (field "meaning" string)
        (maybe (field "onyomi" string))
        (maybe (field "kunyomi" string))
        (maybe (at [ "user_specific", "srs_numeric" ] int))


requestDecode =
    field "requested_information" (list kanjiDecode)


allLevels =
    List.range 1 60 |> List.map toString |> String.join ","


getData : Cmd Message
getData =
    let
        url =
            "https://www.wanikani.com/api/user/5e63183f2d2d844f0aef5c0665676a71/kanji/" ++ allLevels

        req =
            Http.get url requestDecode
    in
    Http.send HttpAnswer req
