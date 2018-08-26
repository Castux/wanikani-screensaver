module Api exposing (getData)

import Http
import Json.Decode exposing (..)
import KanjiData exposing (KanjiData)


computePercentage read_ok read_nok mean_ok mean_nok =
    Maybe.map4
        (\a b c d -> (toFloat a + toFloat c) / (toFloat a + toFloat b + toFloat c + toFloat d))
        read_ok
        read_nok
        mean_ok
        mean_nok


kanjiDecode =
    map7 (\level character srs read_ok read_nok mean_ok mean_nok -> KanjiData level character srs (computePercentage read_ok read_nok mean_ok mean_nok))
        (field "level" int)
        (field "character" string)
        (maybe (at [ "user_specific", "srs_numeric" ] int))
        (maybe (at [ "user_specific", "reading_correct" ] int))
        (maybe (at [ "user_specific", "reading_incorrect" ] int))
        (maybe (at [ "user_specific", "meaning_correct" ] int))
        (maybe (at [ "user_specific", "meaning_incorrect" ] int))


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
