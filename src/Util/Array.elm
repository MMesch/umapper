module Util.Array exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import List


getIdx : a -> Array a -> Maybe Int
getIdx value arr =
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


traverse : (a -> Maybe b) -> Array a -> Maybe (Array b)
traverse f =
    A.foldr (\x -> Maybe.map2 (::) (f x)) (Just []) >> Maybe.map A.fromList


maximum : Array comparable -> Maybe comparable
maximum =
    List.maximum << A.toList


minimum : Array comparable -> Maybe comparable
minimum =
    List.minimum << A.toList


add : Array number -> Array number -> Array number
add =
    A.map2 (+)


mult : Array number -> Array number -> Array number
mult =
    A.map2 (*)


setAll : number -> Array number -> Array number
setAll num =
    A.map (\_ -> num)
