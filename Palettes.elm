module Palettes exposing (..)

import Array
import Color
import Utils


wanikani level =
    [ "black"
    , "#dd0093"
    , "#dd0093"
    , "#dd0093"
    , "#dd0093"
    , "#882d9e"
    , "#882d9e"
    , "#294ddb"
    , "#0093dd"
    ]
        |> Array.fromList
        |> Array.get level
        |> Maybe.withDefault "#222222"


grayscale level =
    1 - toFloat level / 8.0 |> Color.grayscale |> Utils.cssColor
