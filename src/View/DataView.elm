module View.DataView exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import Css
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Lazy exposing (lazy2)
import Matrix
import Maybe exposing (withDefault)
import Model.Model exposing (Model, Msg(..))
import Svg
import Util
import View.Components exposing (Component, onChange, panel)
import View.DataViz


viewPanel : Model -> Component
viewPanel model =
    let
        colidx =
            withDefault 0 (Matrix.getColumnId "name" model.headers)

        col =
            Matrix.getColumn colidx "" model.records
    in
    panel
        [ css
            [ Css.width (Css.vw 100)
            , Css.height (Css.vh 90)
            ]
        ]
        [ sliders model, lazy2 graphMap model.positions col ]


sliders : Model -> Component
sliders model =
    div [ css [ Css.position Css.fixed ] ]
        (List.map2 indexSlider (A.toIndexedList model.headers) (A.toList model.weights))


indexSlider : ( Int, String ) -> Float -> Component
indexSlider ( index, label_ ) value =
    div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
        [ label [] [ text label_ ]
        , input
            [ Att.type_ "range"
            , Att.min "0.0"
            , Att.max "1.0"
            , Att.step "0.1"
            , Att.value <| String.fromFloat value
            , onChange (UpdateWeight index)
            ]
            [ text <| String.fromFloat value ]
        ]


graphMap : Maybe Matrix.Matrix -> Maybe (Array String) -> Component
graphMap positions labels =
    let
        data =
            withDefault Util.testMatrix positions

        labels_ =
            withDefault (A.repeat (A.length data) "NA") labels
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
                [ View.DataViz.graph data labels_ ]
        ]
