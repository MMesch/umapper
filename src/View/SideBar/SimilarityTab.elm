module View.SideBar.SimilarityTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Model.Model exposing (ColumnParams, Msg(..))
import Util.Cmap
import View.Components exposing (Component, onChange, theme)
import View.SideBar.Style exposing (Layout(..), reusableInput, reusableSelect, reusableTab)


similarityTab : Array ColumnParams -> Component
similarityTab columnParams =
    reusableTab { title = "Similarity", layout = RowLayout }
        (List.map
            indexSlider
            (A.toIndexedList columnParams)
        )


indexSlider : ( Int, ColumnParams ) -> Component
indexSlider ( index, params ) =
    div [ css [ Css.display Css.inlineBlock ] ]
        [ div
            [ css
                [ Css.margin (Css.px 5)
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.border3 (Css.px 1) Css.solid theme.black
                ]
            ]
            [ div
                [ css
                    [ Css.textAlign Css.center
                    , Css.color theme.white
                    , Css.margin (Css.px 5)
                    ]
                ]
                [ text params.name ]
            , reusableSelect
                (SetColumnDistance index)
                { title = "distance"
                , selected = Model.Model.toString params.distance
                , values = List.map Tuple.first Model.Model.distanceMap
                }
            , reusableInput "weight"
                [ Att.type_ "range"
                , Att.min "0.0"
                , Att.max "1.0"
                , Att.step "0.1"
                , Att.value <| String.fromFloat params.weight
                , onChange (SetColumnWeight index)
                ]
            ]
        ]
