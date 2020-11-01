module View.Graph exposing (..)

import Array as A
import Array.Extra as A
import Css exposing (matrix, translateY)
import Maybe exposing (withDefault)
import Maybe.Extra
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util.Matrix exposing (Matrix)
import Util.Util exposing (normalize)


graph :
    { positions : Matrix
    , labels : A.Array String
    , colors : A.Array String
    , sizes : A.Array Float
    , center : ( Float, Float )
    , zoom : Float
    }
    -> Svg msg
graph { positions, labels, colors, sizes, center, zoom } =
    let
        panning =
            "translate("
                ++ String.fromFloat (-1 * Tuple.first center)
                ++ ", "
                ++ String.fromFloat (-1 * Tuple.second center)
                ++ ")"

        zooming =
            "scale(" ++ String.fromFloat zoom ++ ")"
    in
    svg
        [ viewBox "-1 -1 101 101" ]
        [ g [ transform (zooming ++ " " ++ panning) ]
            (A.toList <|
                A.map4 toNode (normalize positions) labels sizes colors
            )
        ]


toNode : A.Array Float -> String -> Float -> String -> Svg msg
toNode position label size color =
    let
        px =
            safeRetrieve 0 position

        py =
            safeRetrieve 1 position
    in
    nodeGroup px py label size color


safeRetrieve idx =
    String.fromFloat << (*) 100 << Maybe.withDefault 0 << A.get idx


nodeGroup : String -> String -> String -> Float -> String -> Svg msg
nodeGroup px py label size color =
    let
        radius_ =
            String.fromFloat <| size * 0.5

        fontSize_ =
            String.fromFloat size

        textPos =
            "translate(0," ++ String.fromFloat size ++ ")"
    in
    g []
        [ circle [ cx px, cy py, r radius_, fill color ] []
        , text_ [ x px, y py, transform textPos, fontSize fontSize_, fill color ] [ text label ]
        ]
