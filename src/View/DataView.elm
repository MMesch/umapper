module View.DataView exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import Css
import Dict
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Lazy exposing (lazy, lazy2, lazy3)
import List as L
import Matrix
import Maybe exposing (withDefault)
import Maybe.Extra
import Model.Model exposing (ColumnParams, Model, Msg(..))
import Set
import Svg
import Util
import View.Cmap
import View.Components exposing (Component, onChange, panel)
import View.DataViz
import View.SimilarityControl


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
            Maybe.andThen (\channel -> Matrix.getColumnId channel headers) model.plotParams.colorChannel

        colorColumn =
            Maybe.andThen (\idx -> Matrix.getColumn idx "" model.records) colorsidx
    in
    panel [ css [ Css.flexGrow (Css.num 1) ] ]
        [ div
            [ css
                [ Css.height (Css.vh 90)
                ]
            ]
            [ lazy View.SimilarityControl.similarityControl model.columnParams
            , lazy3 graphMap model.positions labels (Maybe.map translate colorColumn)
            ]
        ]


translate : Array String -> Array String
translate values =
    let
        unique =
            (A.toList >> Set.fromList >> Set.toList) values

        mapping =
            Dict.fromList <| L.map2 (\x y -> ( x, y )) unique View.Cmap.qualitative
    in
    A.map (\a -> withDefault "#000000" <| Dict.get a mapping) values


graphMap : Maybe Matrix.Matrix -> Array String -> Maybe (Array String) -> Component
graphMap positions labels colors =
    let
        data =
            withDefault Util.testMatrix positions

        labels_ =
            labels

        colors_ =
            withDefault (A.repeat (A.length data) "#000000") colors
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
                [ Html.Attributes.style "width" "95%"
                , Html.Attributes.style "height" "95%"
                , Html.Attributes.id "graph"
                ]
                [ View.DataViz.graph data labels_ colors_ ]
        ]
