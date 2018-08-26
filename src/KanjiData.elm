module KanjiData exposing (KanjiData)


type alias KanjiData =
    { level : Int
    , character : String
    , srs : Maybe Int
    , correct : Maybe Float
    }
