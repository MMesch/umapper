module Util exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import List as L
import Matrix exposing (Matrix)
import Maybe exposing (withDefault)
import Maybe.Extra
import Set
import String


simpleCompare : String -> String -> Int
simpleCompare s1 s2 =
    if s1 == s2 then
        1

    else
        0


compareMultiples : String -> String -> String -> Float
compareMultiples separator s1 s2 =
    let
        set1 =
            Set.fromList <| String.split separator s1

        set2 =
            Set.fromList <| String.split separator s2
    in
    toFloat <| Set.size (Set.intersect set1 set2)


compareColumn : Array String -> Matrix
compareColumn col =
    A.map (\el -> A.map (compareMultiples ";" el) col) col


normalizedCompareWithWeight : Float -> Array String -> Maybe Matrix
normalizedCompareWithWeight w col =
    let
        nelements =
            A.length col
    in
    if Debug.log "weight: " (w < 0.01) then
        Nothing

    else
        Just <| (compareColumn >> normalize >> Matrix.multScalar w) col


sumSquareMatrices : Matrix -> Array (Maybe Matrix) -> Matrix
sumSquareMatrices init matrices =
    A.foldl
        (\a b ->
            case a of
                Nothing ->
                    b

                Just a_ ->
                    Matrix.add a_ b
        )
        init
        matrices


compareColumns : Array Float -> Array (Array String) -> Matrix
compareColumns weights rows =
    let
        ( n1, n2 ) =
            Matrix.shape rows

        zeros =
            Matrix.repeat 0 ( n1, n1 )

        columns =
            Matrix.transposeWithDef "NA" rows
    in
    sumSquareMatrices zeros <| A.map2 normalizedCompareWithWeight weights columns


testData =
    [ [ 1, 2, 3 ]
    , [ 4, 1, 3 ]
    , [ 1, 2, 3 ]
    , [ 4.1, 2.1, 3.1 ]
    , [ 4.3, 2, 3.2 ]
    , [ 4, 2.2, 3.1 ]
    , [ 4, 2, 3.0 ]
    , [ 4, 2, 3.1 ]
    , [ 4, 2, 3.2 ]
    , [ 4, -2, 3 ]
    , [ 4, 0, 3 ]
    , [ 4, 1, 3 ]
    , [ 4, 3, 3 ]
    , [ 5, 1, 3 ]
    , [ 4.3, 2, 3 ]
    , [ 4.1, 2, 3 ]
    ]


testMatrix =
    Matrix.asArrayMatrix testData


normalize : Matrix -> Matrix
normalize matrix =
    let
        max =
            withDefault 0.000001 <| Matrix.maximum matrix

        min =
            withDefault 0.000001 <| Matrix.minimum matrix

        range =
            max - min

        normalizeValue value =
            (value - min) / range
    in
    Matrix.map normalizeValue matrix
