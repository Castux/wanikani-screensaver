module View exposing (view)

import Debug
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


type alias Layout =
    { w : Int
    , h : Int
    , tileSize : Float
    , delta : ( Float, Float )
    }


third ( a, b, c ) =
    c


background windowSize =
    rect
        [ x "0"
        , y "0"
        , fill "black"
        , width <| toString windowSize.width
        , height <| toString windowSize.height
        ]
        []


svgWrap windowSize =
    let
        w =
            toString windowSize.width

        h =
            toString windowSize.height
    in
    svg
        [ viewBox ("0 0 " ++ w ++ " " ++ h)
        , width w
        , height h
        ]


computeLayout state =
    let
        margin =
            10.0

        ( ww, wh ) =
            ( toFloat state.windowSize.width - 2 * margin
            , toFloat state.windowSize.height - 2 * margin
            )

        ( w, h ) =
            bestFit (ww / wh) (List.length state.kanjis)

        tileSize =
            ww / toFloat w

        delta =
            ( (ww - toFloat w * tileSize) / 2.0 + margin
            , (wh - toFloat h * tileSize) / 2.0 + margin
            )
    in
    Layout w h tileSize delta


view state =
    let
        layout =
            computeLayout state

        font =
            floor layout.tileSize - 1 |> toString
    in
    state.kanjis
        |> List.sortBy .character
        |> List.indexedMap (viewKanji layout)
        |> g [ fontFamily "Noto Sans CJK JP", fontSize font, dominantBaseline "middle", textAnchor "middle" ]
        |> List.singleton
        |> (++) [ background state.windowSize ]
        |> svgWrap state.windowSize


kanjiColor k =
    case k.srs of
        Nothing ->
            "#222222"

        Just level ->
            let
                interp = 255 * level // 8 |> toString
            in
                "rgb(" ++ interp ++ "," ++ interp ++ "," ++ interp ++ ")"


viewKanji layout index kanji =
    let
        ( dx, dy ) =
            layout.delta

        sx =
            (index % layout.w)
                |> toFloat
                |> (+) 0.5
                |> (*) layout.tileSize
                |> (+) dx
                |> toString

        sy =
            (index // layout.w)
                |> toFloat
                |> (+) 0.5
                |> (*) layout.tileSize
                |> (+) dy
                |> toString
    in
    text_
        [ x sx, y sy, fill (kanjiColor kanji) ]
        [ text kanji.character ]



-- w * h = N && w / h = ratio
-- w = N/h && w = ratio*h
-- N/h = ratio*h
-- h = sqrt(N/ratio)


bestFit ratio n =
    let
        fn =
            toFloat n

        h =
            fn / ratio |> sqrt |> floor

        w =
            fn / toFloat h |> ceiling
    in
    ( w, h )
