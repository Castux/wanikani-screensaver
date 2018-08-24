module Layout exposing (computeLayout)

import Random
import Random.List
import Set exposing (Set)
import Utils exposing (pairMap, pairRange)



-- Grid represents all the remaining free cells


type alias Grid =
    Set ( Int, Int )


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
        |> Set.fromList


freeCell : Grid -> Int -> ( Int, Int ) -> Bool
freeCell grid size ( x, y ) =
    case size of
        1 ->
            Set.member ( x, y ) grid

        _ ->
            pairRange ( x, y ) ( x + size - 1, y + size - 1 )
                |> List.all (\pos -> Set.member pos grid)


freeCells : Grid -> Int -> List ( Int, Int )
freeCells grid size =
    let
        list =
            Set.toList grid
    in
    case size of
        1 ->
            list

        _ ->
            List.filter (freeCell grid size) list


insertInGrid : Grid -> Int -> ( Int, Int ) -> Grid
insertInGrid grid size ( x, y ) =
    case size of
        1 ->
            Set.remove ( x, y ) grid

        _ ->
            pairRange ( x, y ) ( x + size - 1, y + size - 1 )
                |> Set.fromList
                |> Set.diff grid


randomFill : Grid -> List ( a, Int ) -> Random.Seed -> List ( a, Int, ( Int, Int ) )
randomFill emptyGrid allItems initialSeed =
    let
        rec grid items seed result =
            case items of
                [] ->
                    result

                ( item, size ) :: rest ->
                    let
                        spots =
                            freeCells grid size

                        ( ( maybeSpot, _ ), newSeed ) =
                            Random.step (Random.List.choose spots) seed
                    in
                    case maybeSpot of
                        Nothing ->
                            result

                        Just spot ->
                            rec
                                (insertInGrid grid size spot)
                                rest
                                newSeed
                                (( item, size, spot ) :: result)
    in
    rec emptyGrid allItems initialSeed []



-- The algorithm attempts to place the objects in the order they are given,
-- so it works better when larger ones come first, and filling (size 1) comes at
-- the end.


computeLayout : List ( a, Int ) -> Float -> Random.Seed -> ( List ( a, Int, ( Int, Int ) ), ( Int, Int ) )
computeLayout items aspectRatio seed =
    let
        surface =
            items
                |> List.map (Tuple.second >> (\n -> n * n))
                |> List.sum

        ( w, h ) =
            bestFit surface aspectRatio

        grid =
            makeGrid ( w, h )
    in
    ( randomFill grid items seed, ( w, h ) )
