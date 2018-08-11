module Layout exposing (computeLayout)

import Dict exposing (Dict)
import Utils exposing (pairMap, pairRange)


type Cell
    = Empty
    | Corner Int
    | Blocked


type alias Grid =
    Dict ( Int, Int ) Cell


bestFit : Int -> Float -> ( Int, Int )
bestFit numItems aspectRatio =
    let
        fn =
            toFloat numItems

        h =
            fn / aspectRatio |> sqrt |> floor

        w =
            fn / toFloat h |> ceiling
    in
    ( w, h )


makeGrid : ( Int, Int ) -> Grid
makeGrid ( w, h ) =
    pairRange ( 0, 0 ) ( w, h )
        |> List.map (\x -> ( x, Empty ))
        |> Dict.fromList


freeCell : Grid -> Int -> ( Int, Int ) -> Bool
freeCell grid size ( x, y ) =
    pairRange ( 0, 0 ) ( size - 1, size - 1 )
        |> List.map (pairMap ((+) x) ((+) y))
        |> List.map (flip Dict.get grid)
        |> List.all ((==) (Just Empty))


freeCells : Grid -> Int -> List ( Int, Int )
freeCells grid size =
    Dict.keys grid
        |> List.filter (freeCell grid size)


computeLayout : List ( a, Int ) -> Float -> List ( a, Int, ( Int, Int ) )
computeLayout items aspectRatio =
    let
        surface =
            items |> List.map (Tuple.second >> (\n -> n * n)) |> List.sum

        ( w, h ) =
            bestFit surface aspectRatio

        sorted =
            List.sortBy Tuple.second items |> List.reverse
    in
    items
        |> List.map (\( item, size ) -> ( item, size, ( 0, 0 ) ))
