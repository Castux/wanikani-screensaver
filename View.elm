module View exposing (updateTiles, view)

import Array
import Debug
import Html
import Html.Attributes
import Palettes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Types exposing (..)
import Utils


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
        , Html.Attributes.style [ ( "margin", "auto" ), ( "width", "100%" ), ( "height", "100%" ) ]
        ]


computeLayout windowSize numTiles =
    let
        margin =
            10.0

        ( ww, wh ) =
            ( toFloat windowSize.width - 2 * margin
            , toFloat windowSize.height - 2 * margin
            )

        ( w, h ) =
            bestFit (ww / wh) numTiles

        tileSize =
            ww / toFloat w

        delta =
            ( (ww - toFloat w * tileSize) / 2.0 + margin
            , (wh - toFloat h * tileSize) / 2.0 + margin
            )
    in
    Layout w h tileSize delta


view : State -> Html.Html msg
view state =
    let
        layout =
            computeLayout state.windowSize (List.length state.tiles)

        font =
            floor layout.tileSize - 1 |> toString
    in
    state.tiles
        |> List.map viewTile
        |> g
            [ fontFamily "Noto Sans CJK JP"
            , fontSize font
            , dominantBaseline "middle"
            , textAnchor "middle"
            ]
        |> List.singleton
        |> (++) [ background state.windowSize ]
        |> svgWrap state.windowSize
        |> List.singleton
        |> Html.div
            [ width "100%"
            , height "100%"
            , Html.Attributes.style
                [ ( "display", "flex" )
                , ( "min-height", "100%" )
                , ( "background", "black" )
                ]
            ]


kanjiColor k =
    case k.srs of
        Nothing ->
            "#202020"

        Just level ->
            Palettes.wanikani level


computeGridPos layout index =
    ( index % layout.w, index // layout.w )


computeRealPos layout ( gx, gy ) =
    let
        ( dx, dy ) =
            layout.delta

        sx =
            gx
                |> toFloat
                |> (+) 0.5
                |> (*) layout.tileSize
                |> (+) dx

        sy =
            gy
                |> toFloat
                |> (+) 0.5
                |> (*) layout.tileSize
                |> (+) dy
    in
    ( sx, sy )


viewTile : Tile -> Svg msg
viewTile tile =
    case tile.realPos of
        Just pos ->
            let
                ( cx, cy ) =
                    pos
                        |> Tuple.mapFirst toString
                        |> Tuple.mapSecond toString
            in
            text_
                [ x cx, y cy, fill (kanjiColor tile.kanji) ]
                [ text tile.kanji.character ]

        Nothing ->
            text_ [] []



-- w * h = N && w / h = ratio
-- w = N/h && w = ratio*h
-- N/h = ratio*h
-- h = sqrt(N/ratio)


bestFit : Float -> Int -> ( Int, Int )
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


updateTiles : Time.Time -> State -> List Tile
updateTiles dt state =
    let
        layout =
            computeLayout state.windowSize (List.length state.tiles)
    in
    List.indexedMap (updateTile dt layout) state.tiles


updateTile : Time.Time -> Layout -> Int -> Tile -> Tile
updateTile dt layout index tile =
    let
        ( tx, ty ) =
            computeGridPos layout index |> computeRealPos layout

        speed =
            0.001

        newPos =
            case tile.realPos of
                Just ( x, y ) ->
                    ( x + (tx - x) * speed * dt
                    , y + (ty - y) * speed * dt
                    )

                Nothing ->
                    ( tx, ty )
    in
    { tile | realPos = Just newPos }
