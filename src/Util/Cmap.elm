module Util.Cmap exposing (..)

import Array as A exposing (Array)
import Dict
import List
import Maybe
import Set
import Util.Util


type Colormap
    = Qualitative
    | Quantitative
    | Diverging


colormapMap : List ( String, Colormap )
colormapMap =
    [ ( "Qualitative", Qualitative )
    , ( "Quantitative", Quantitative )
    , ( "Diverging", Diverging )
    ]


toString : Colormap -> String
toString =
    Util.Util.typeMapToString colormapMap


qualitative =
    [ "#a6cee3"
    , "#1f78b4"
    , "#b2df8a"
    , "#33a02c"
    , "#fb9a99"
    , "#e31a1c"
    , "#fdbf6f"
    , "#ff7f00"
    , "#cab2d6"
    , "#6a3d9a"
    , "#ffff99"
    , "#b15928"
    ]


translate : Array String -> Array String
translate values =
    let
        unique =
            (A.toList >> Set.fromList >> Set.toList) values

        mapping =
            Dict.fromList <| List.map2 (\x y -> ( x, y )) unique qualitative
    in
    A.map (\a -> Maybe.withDefault "#000000" <| Dict.get a mapping) values
