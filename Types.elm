module Types exposing (..)

import Http
import Window


type alias Kanji =
    { level : Int
    , character : String
    , meaning : String
    , onyomi : Maybe String
    , kunyomi : Maybe String
    , srs : Maybe Int
    }


type alias State =
    { windowSize : Window.Size
    , kanjis : List Kanji
    }


type Message
    = Nop
    | HttpAnswer (Result Http.Error (List Kanji))
    | WindowResize Window.Size
