module Layout exposing (computeLayout)

import Dict exposing (Dict)
import Random
import Utils exposing (pairMap, pairRange, randomListElement)


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
            fn / aspectRatio |> sqrt

        w =
            fn / h
    in
    ( floor w, floor h )


makeGrid : ( Int, Int ) -> Grid
makeGrid ( w, h ) =
    pairRange ( 0, 0 ) ( w - 1, h - 1 )
        |> List.map (\x -> ( x, Empty ))
        |> Dict.fromList


freeCell : Grid -> Int -> ( Int, Int ) -> Bool
freeCell grid size ( x, y ) =
    case size of
        1 ->
            Dict.get ( x, y ) grid == Just Empty

        _ ->
            pairRange ( x, y ) ( x + size - 1, y + size - 1 )
                |> List.map (flip Dict.get grid)
                |> List.all ((==) (Just Empty))


freeCells : Grid -> Int -> List ( Int, Int )
freeCells grid size =
    Dict.keys grid
        |> List.filter (freeCell grid size)


insertInGrid : Grid -> Int -> ( Int, Int ) -> Grid
insertInGrid grid size ( x, y ) =
    case size of
        1 ->
            Dict.insert ( x, y ) (Corner 1) grid

        _ ->
            pairRange ( x, y ) ( x + size - 1, y + size - 1 )
                |> List.foldr (\p -> Dict.insert p Blocked) grid
                |> Dict.insert ( x, y ) (Corner size)


randomFill : Grid -> List ( a, Int ) -> Random.Seed -> List ( a, Int, ( Int, Int ) )
randomFill emptyGrid items initialSeed =
    let
        rec grid items seed result =
            case items of
                [] ->
                    result

                ( item, size ) :: rest ->
                    let
                        spots =
                            freeCells grid size

                        ( spot, newSeed ) =
                            randomListElement spots seed
                    in
                    case spot of
                        Nothing ->
                            result

                        Just spot ->
                            rec (insertInGrid grid size spot) rest newSeed (( item, size, spot ) :: result)
    in
    rec emptyGrid items initialSeed []


computeLayout : List ( a, Int ) -> Float -> Random.Seed -> ( List ( a, Int, ( Int, Int ) ), ( Int, Int ) )
computeLayout items aspectRatio seed =
    let
        surface =
            items |> List.map (Tuple.second >> (\n -> n * n)) |> List.sum

        ( w, h ) =
            bestFit surface aspectRatio

        sorted =
            List.sortBy Tuple.second items |> List.reverse

        grid =
            makeGrid ( w, h )
    in
    ( randomFill grid sorted seed, ( w, h ) )
