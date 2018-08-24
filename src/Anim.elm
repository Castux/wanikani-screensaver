module Anim exposing (grayscale, plasma, toCss)


plasma : Float -> Float -> Float -> Float
plasma x y t =
    (sin (x / 100.0 + t / 1000.0) + 1.0) / 2.0


grayscale : Float -> ( Float, Float, Float )
grayscale x =
    ( x, x, x )


toCss ( r, g, b ) =
    "rgb("
        ++ (r * 255.0 |> floor |> String.fromInt)
        ++ ","
        ++ (g * 255.0 |> floor |> String.fromInt)
        ++ ","
        ++ (b * 255.0 |> floor |> String.fromInt)
        ++ ")"
