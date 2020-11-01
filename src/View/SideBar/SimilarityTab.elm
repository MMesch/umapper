module View.SideBar.SimilarityTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Model.Model exposing (ColumnParams, Msg(..))
import Util.Cmap
import View.Components exposing (Component, onChange, theme)
import View.SideBar.Style
    exposing
        ( Layout(..)
        , reusableInput
        , reusableSelect
        , reusableTab
        )


similarityTab : Array ColumnParams -> Component
similarityTab columnParams =
    reusableTab { title = "Similarity", layout = RowLayout }
        [ div
            [ css
                [ Css.property "display" "grid"
                , Css.alignItems Css.center
                , Css.overflowX Css.auto
                , Css.property "column-gap" "5px"
                , Css.property "row-gap" "10px"
                , Css.paddingBottom (Css.px 10)
                , Css.paddingTop (Css.px 10)
                ]
            ]
            (firstColumn [ "", "distance", "weight" ]
                ++ List.concatMap columnBuilder (A.toIndexedList columnParams)
            )
        ]


firstColumn : List String -> List Component
firstColumn labels =
    List.indexedMap
        (\i t ->
            div
                [ css
                    [ Css.property "grid-column" "1"
                    , Css.property "grid-row" (String.fromInt (i + 1))
                    , Css.color theme.white
                    , Css.display Css.block
                    ]
                ]
                [ text t ]
        )
        labels


columnBuilder : ( Int, ColumnParams ) -> List Component
columnBuilder ( index, params ) =
    let
        currentColumn =
            Css.property "grid-column" (String.fromInt (index + 2))
    in
    [ div
        [ css
            [ currentColumn
            , Css.display Css.block
            , Css.property "grid-row" "1"
            , Css.color theme.white
            , Css.padding (Css.px 2)
            ]
        ]
        [ text params.name
        ]
    , div
        [ css
            [ Css.display Css.block
            , currentColumn
            , Css.property "grid-row" "2"
            ]
        ]
        [ reusableSelect
            (SetColumnDistance index)
            { title = Nothing
            , hasEmpty = False
            , selected = Just <| Model.Model.toString params.distance
            , values = List.map Tuple.first Model.Model.distanceMap
            }
        ]
    , div
        [ css
            [ Css.display Css.block
            , Css.property "grid-row" "3"
            , currentColumn
            ]
        ]
        [ reusableInput Nothing
            [ Att.type_ "range"
            , Att.min "0.0"
            , Att.max "1.0"
            , Att.step "0.1"
            , Att.value <| String.fromFloat params.weight
            , onChange (SetColumnWeight index)
            ]
        ]
    ]
