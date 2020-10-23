module Matrix exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import Html exposing (a)
import List
import Maybe exposing (withDefault)
import Maybe.Extra



-- Matrix functions


type alias Matrix =
    Array (Array Float)


minimum : Matrix -> Maybe Float
minimum arrs =
    Maybe.andThen arrmin (arrtraverse arrmin arrs)


maximum : Matrix -> Maybe Float
maximum arrs =
    Maybe.andThen arrmax (arrtraverse arrmax arrs)


map : (Float -> Float) -> Matrix -> Matrix
map func =
    A.map (A.map func)


map2 : (Float -> Float -> Float) -> Matrix -> Matrix -> Matrix
map2 func =
    A.map2 (A.map2 func)


add : Matrix -> Matrix -> Matrix
add =
    map2 (+)


mult : Matrix -> Matrix -> Matrix
mult =
    map2 (*)


multScalar : Float -> Matrix -> Matrix
multScalar c =
    map ((*) c)


asArrayMatrix : List (List a) -> Array (Array a)
asArrayMatrix ll =
    A.fromList (List.map A.fromList ll)


get : ( Int, Int ) -> Array (Array a) -> Maybe a
get ( i1, i2 ) =
    A.get i1 >> withDefault A.empty >> A.get i2


set : ( Int, Int ) -> a -> Array (Array a) -> Array (Array a)
set ( i1, i2 ) value m =
    A.set i1 (A.set i2 value (withDefault A.empty <| A.get i1 m)) m


shape : Array (Array a) -> ( Int, Int )
shape m =
    let
        l2 =
            A.length <| withDefault A.empty <| A.get 0 m

        l1 =
            A.length m
    in
    ( l1, l2 )


transposeWithDef : a -> Array (Array a) -> Array (Array a)
transposeWithDef def m =
    let
        ( l1, l2 ) =
            shape m

        setFromTp matrix ( i1, i2 ) =
            set ( i1, i2 ) (withDefault def <| get ( i2, i1 ) matrix)

        empty =
            repeat def ( l2, l1 )
    in
    List.foldl (setFromTp m) empty (permutations ( l1, l2 ))


repeat : a -> ( Int, Int ) -> Array (Array a)
repeat value ( l1, l2 ) =
    A.repeat l1 (A.repeat l2 value)


setAll : Float -> Matrix -> Matrix
setAll num =
    map (\_ -> num)


permutations : ( Int, Int ) -> List ( Int, Int )
permutations ( l1, l2 ) =
    List.concatMap (\el -> List.map (\b -> ( el, b )) (List.range 0 l1)) (List.range 0 l2)


getColumn : Int -> a -> Array (Array a) -> Maybe (Array a)
getColumn idx def matrix =
    A.get idx (transposeWithDef def matrix)



-- Array helpers


getColumnId : a -> Array a -> Maybe Int
getColumnId value arr =
    List.foldr
        (\( inew, v ) iold ->
            case iold of
                Just ix ->
                    Just ix

                Nothing ->
                    if value == v then
                        Just inew

                    else
                        Nothing
        )
        Nothing
        (A.toIndexedList arr)


arrtraverse : (a -> Maybe b) -> Array a -> Maybe (Array b)
arrtraverse f =
    A.foldr (\x -> Maybe.map2 (::) (f x)) (Just []) >> Maybe.map A.fromList


arrmax : Array comparable -> Maybe comparable
arrmax =
    List.maximum << A.toList


arrmin : Array comparable -> Maybe comparable
arrmin =
    List.minimum << A.toList


arradd : Array number -> Array number -> Array number
arradd =
    A.map2 (+)


arrmult : Array number -> Array number -> Array number
arrmult =
    A.map2 (*)


arrsetAll : number -> Array number -> Array number
arrsetAll num =
    A.map (\_ -> num)
