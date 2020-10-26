module View.SimilarityControl exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Model.Model exposing (ColumnParams, Msg(..))
import View.Components exposing (Component, onChange)


similarityControl : Array ColumnParams -> Component
similarityControl columnParams =
    div [ css [ Css.position Css.fixed ] ]
        (h2 [] [ text "Similarity" ] :: List.map indexSlider (A.toIndexedList columnParams))


indexSlider : ( Int, ColumnParams ) -> Component
indexSlider ( index, params ) =
    div [ css [ Css.margin (Css.px 5), Css.displayFlex, Css.flexDirection Css.column ] ]
        [ label [] [ text params.name ]
        , select [ Att.name "distance function" ]
            [ option [ Att.value "MultiString" ] [ text "MultiString" ] ]
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
