module View.SideBar.SimilarityTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Model.Model exposing (ColumnParams, Msg(..))
import Util.Cmap
import View.Components exposing (Component, onChange, theme)
import View.SideBar.Style exposing (Layout(..), reusableTab)


similarityTab : Array ColumnParams -> Component
similarityTab columnParams =
    reusableTab { title = "Similarity", layout = RowLayout }
        (List.map
            indexSlider
            (A.toIndexedList columnParams)
        )


indexSlider : ( Int, ColumnParams ) -> Component
indexSlider ( index, params ) =
    div [ css [ Css.display Css.block ] ]
        [ div
            [ css
                [ Css.margin (Css.px 5)
                , Css.displayFlex
                , Css.flexDirection Css.column
                ]
            ]
            [ label [ css [ Css.color theme.white ] ] [ text params.name ]
            , select [ Att.name "distance function" ] <|
                List.map
                    (\( n, v ) -> option [ Att.value n ] [ text n ])
                    Util.Cmap.colormapMap
            , input
                [ Att.type_ "range"
                , Att.min "0.0"
                , Att.max "1.0"
                , Att.step "0.1"
                , Att.value <| String.fromFloat params.weight
                , onChange (UpdateWeight index)
                ]
                [ text <| String.fromFloat params.weight ]
            ]
        ]
