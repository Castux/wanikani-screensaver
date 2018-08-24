module Anim exposing (grayscale, plasma, toCss)


sine x =
    sin (turns x)


cose x =
    cos (turns x)


plasma : Float -> Float -> Float -> Float
plasma x y t =
    let
        components =
            [ sine (x / 45.0 + t / 8.0)
            , cose ((x * sine (t / 20.0) + y * cose (t / 18.0)) / 35.0 + t / 10.0)
            ]

        avg =
            List.sum components / toFloat (List.length components)
    in
    avg


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
