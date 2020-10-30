module View.Graph exposing (..)

import Array as A
import Array.Extra as A
import Css exposing (matrix, translateY)
import Maybe exposing (withDefault)
import Maybe.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util.Matrix exposing (Matrix)
import Util.Util exposing (normalize)


graph :
    { positions : Matrix
    , labels : A.Array String
    , colors : A.Array String
    , sizes : A.Array String
    }
    -> Svg msg
graph { positions, labels, colors, sizes } =
    svg
        [ viewBox "-1 -1 101 101"
        ]
        (A.toList <|
            A.map3 toNode (normalize positions) labels colors
        )


toNode : A.Array Float -> String -> String -> Svg msg
toNode position label color =
    let
        px =
            safeRetrieve 0 position

        py =
            safeRetrieve 1 position
    in
    nodeGroup px py label color


safeRetrieve idx =
    String.fromFloat << (*) 100 << Maybe.withDefault 0 << A.get idx


nodeGroup : String -> String -> String -> String -> Svg msg
nodeGroup px py label color =
    g []
        [ circle [ cx px, cy py, r "0.5", fill color ] []
        , text_ [ x px, y py, transform "translate(0 -1)", fontSize "1", fill color ] [ text label ]
        ]
