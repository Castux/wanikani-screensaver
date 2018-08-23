module KanjiData exposing (..)


type alias KanjiData =
    { level : Int
    , character : String
    , meaning : String
    , onyomi : Maybe String
    , kunyomi : Maybe String
    , srs : Maybe Int
    }
