module View.DataView exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import Css
import Dict
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Lazy exposing (lazy2, lazy3)
import List as L
import Matrix
import Maybe exposing (withDefault)
import Model.Model exposing (Model, Msg(..))
import Set
import Svg
import Util
import View.Cmap
import View.Components exposing (Component, onChange, panel)
import View.DataViz


viewPanel : Model -> Component
viewPanel model =
    let
        labelsidx =
            withDefault 0 (Matrix.getColumnId "name" model.headers)

        labels =
            Matrix.getColumn labelsidx "" model.records

        colorsidx =
            withDefault 0 (Matrix.getColumnId "ctype" model.headers)

        colorColumn =
            Matrix.getColumn colorsidx "" model.records
    in
    panel [ css [ Css.flexGrow (Css.num 1) ] ]
        [ div
            [ css
                [ Css.height (Css.vh 90)
                ]
            ]
            [ sliders model
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


sliders : Model -> Component
sliders model =
    div [ css [ Css.position Css.fixed ] ]
        (List.map2 indexSlider (A.toIndexedList model.headers) (A.toList model.weights))


indexSlider : ( Int, String ) -> Float -> Component
indexSlider ( index, label_ ) value =
    div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
        [ input [ Att.type_ "checkbox" ] []
        , label [] [ text label_ ]
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


graphMap : Maybe Matrix.Matrix -> Maybe (Array String) -> Maybe (Array String) -> Component
graphMap positions labels colors =
    let
        data =
            withDefault Util.testMatrix positions

        labels_ =
            withDefault (A.repeat (A.length data) "NA") labels

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
