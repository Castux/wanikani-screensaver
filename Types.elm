module Types exposing (..)

import Array exposing (Array)
import Http
import Time
import Window


type alias Kanji =
    { level : Int
    , character : String
    , meaning : String
    , onyomi : Maybe String
    , kunyomi : Maybe String
    , srs : Maybe Int
    }


type alias Tile =
    { realPos : Maybe ( Float, Float )
    , kanji : Kanji
    }


type alias Layout =
    { w : Int
    , h : Int
    , tileSize : Float
    , delta : ( Float, Float )
    }


type alias State =
    { windowSize : Window.Size
    , tiles : List Tile
    }


type Message
    = Nop
    | HttpAnswer (Result Http.Error (List Kanji))
    | WindowResize Window.Size
    | Tick Time.Time
    | Tock Time.Time
    | SwapTiles ( Int, Int )
    | SetTiles (List Tile)
