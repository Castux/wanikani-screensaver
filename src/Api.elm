module Api exposing (getData)

import Http
import Json.Decode exposing (..)
import KanjiData exposing (KanjiData)


kanjiDecode =
    map6 KanjiData
        (field "level" int)
        (field "character" string)
        (field "meaning" string)
        (maybe (field "onyomi" string))
        (maybe (field "kunyomi" string))
        (maybe (at [ "user_specific", "srs_numeric" ] int))


requestDecode =
    field "requested_information" (list kanjiDecode)


allLevels =
    List.range 1 60 |> List.map String.fromInt |> String.join ","


getData : (Result Http.Error (List KanjiData) -> msg) -> String -> Cmd msg
getData msg key =
    let
        url =
            "https://www.wanikani.com/api/user/" ++ key ++ "/kanji/" ++ allLevels

        req =
            Http.get url requestDecode
    in
    Http.send msg req
