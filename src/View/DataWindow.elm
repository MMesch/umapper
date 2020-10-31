module View.DataWindow exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import Css
import Dict
import Draggable
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Lazy exposing (lazy, lazy2, lazy3)
import List as L
import Maybe exposing (withDefault)
import Maybe.Extra
import Model.Model exposing (ColumnParams, Model, Msg(..))
import Set
import Svg
import Util.Array
import Util.Cmap
import Util.Matrix exposing (Matrix)
import Util.Util
import View.Components exposing (Component, onChange, panel)
import View.Graph


viewPanel : Model -> Component
viewPanel model =
    let
        headers =
            A.map .name model.columnParams

        labels =
            A.map
                (A.zip headers
                    >> A.foldl
                        (\( header, el2 ) a ->
                            if List.any ((==) header) model.plotParams.labelColumns then
                                a ++ " | " ++ el2

                            else
                                a
                        )
                        ""
                )
                model.records

        colorsidx =
            Maybe.andThen (\channel -> Util.Array.getIdx channel headers) model.plotParams.colorChannel

        colorColumn =
            Maybe.andThen (\idx -> Util.Matrix.getColumn idx "" model.records) colorsidx
    in
    panel [ css [ Css.width (Css.pct 100) ] ]
        [ div
            [ css [ Css.height (Css.pct 90) ] ]
            [ lazy graphMap
                { positions = model.positions
                , labels = labels
                , colors = Maybe.map Util.Cmap.translate colorColumn
                , center = model.position
                }
            ]
        ]


graphMap :
    { positions : Maybe Matrix
    , labels : Array String
    , colors : Maybe (Array String)
    , center : ( Int, Int )
    }
    -> Component
graphMap { positions, labels, colors, center } =
    let
        data =
            withDefault Util.Util.testMatrix positions

        labels_ =
            labels

        colors_ =
            withDefault (A.repeat (A.length data) "#000000") colors

        sizes_ =
            withDefault (A.repeat (A.length data) "1") colors
    in
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.display Css.block
            , Css.margin Css.auto
            , Css.overflow Css.auto
            ]
        ]
        [ fromUnstyled <|
            Svg.svg
                ([ Html.Attributes.style "width" "95%"
                 , Html.Attributes.style "height" "95%"
                 , Html.Attributes.id "graph"
                 , Draggable.mouseTrigger "my-element" DragMsg
                 ]
                    ++ Draggable.touchTriggers "my-element" DragMsg
                )
                [ View.Graph.graph
                    { center = center
                    , positions = data
                    , labels = labels_
                    , colors = colors_
                    , sizes = sizes_
                    }
                ]
        ]
