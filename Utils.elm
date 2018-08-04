module Utils exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Random


applyFuncs : List (a -> b) -> a -> List b
applyFuncs =
    (|>) >> List.map |> flip


arraySwap : Int -> Int -> Array elem -> Array elem
arraySwap i1 i2 array =
    Maybe.map2
        (\e1 e2 -> array |> Array.set i1 e2 |> Array.set i2 e1)
        (Array.get i1 array)
        (Array.get i2 array)
        |> Maybe.withDefault array


cssColor : Color -> String
cssColor color =
    let
        tmp =
            color
                |> Color.toRgb
                |> applyFuncs [ .red, .green, .blue ]
                |> List.map toString
                |> String.join ","
    in
    "rgb(" ++ tmp ++ ")"
