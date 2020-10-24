module View.SideBar exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy exposing (lazy3)
import Maybe exposing (withDefault)
import Model.Model exposing (Model, Msg(..))
import Table
import View.Components
    exposing
        ( Builder
        , Component
        , forLargeWidth
        , forSmallWidth
        , onChange
        , panel
        , theme
        )


dataPanel : Model -> Component
dataPanel model =
    panel
        [ css
            [ forLargeWidth
                [ Css.width (Css.pct 30)
                , Css.maxWidth (Css.px 500)
                ]
            ]
        ]
        [ div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                ]
            ]
            [ heyButton model
            , loadButton
            , downloadSvgButton
            , paramInput model
            , dataTable model
            ]
        ]


dataTable : Model -> Component
dataTable model =
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.maxHeight (Css.px 500)
            , Css.display Css.block
            , Css.overflow Css.auto
            , Css.margin (Css.px 2)
            , Css.backgroundColor theme.lightblue
            , Css.marginTop (Css.px 20)
            , Css.borderRadius (Css.px 3)
            ]
        ]
        [ lazy3 table model.headers model.tableState model.records ]


navStyle =
    [ Css.borderRadius (Css.px 2)
    , Css.backgroundColor theme.darkblue
    , Css.color theme.white
    , Css.border (Css.px 0)
    , Css.width (Css.pct 100)
    , Css.display Css.block
    , Css.margin (Css.px 2)
    ]


table : Array String -> Table.State -> Array (Array String) -> Html Msg
table headers tableState records =
    fromUnstyled
        (Table.view
            (Model.Model.tableConfig headers)
            tableState
            (A.toList records)
        )


paramInput : Model -> Component
paramInput model =
    let
        umapParams =
            model.umapParams
    in
    div
        [ css
            (navStyle
                ++ [ Css.displayFlex
                   , Css.flexDirection Css.row
                   , Css.justifyContent Css.spaceBetween
                   ]
            )
        ]
        [ input
            [ Att.type_ "number"
            , Att.placeholder "minDist"
            , Att.step "0.01"
            , Att.value <| String.fromFloat model.umapParams.minDist
            , css [ Css.width (Css.pct 30) ]
            , onInput (\x -> SetUmapParams { umapParams | minDist = withDefault 0.1 (String.toFloat x) })
            ]
            []
        , input
            [ Att.type_ "number"
            , Att.placeholder "spread"
            , Att.step "0.01"
            , Att.value <| String.fromFloat model.umapParams.spread
            , css [ Css.width (Css.pct 30) ]
            , onInput (\x -> SetUmapParams { umapParams | spread = withDefault 1.0 (String.toFloat x) })
            ]
            []
        , input
            [ Att.type_ "number"
            , Att.placeholder "nNeighbours"
            , Att.step "1"
            , Att.value <| String.fromInt model.umapParams.nNeighbors
            , css [ Css.width (Css.pct 30) ]
            , onInput (\x -> SetUmapParams { umapParams | nNeighbors = withDefault 15 (String.toInt x) })
            ]
            []
        ]


menuButton : Builder
menuButton =
    styled
        button
        navStyle


loadButton : Component
loadButton =
    menuButton [ onClick CsvRequested ] [ text "Load CSV" ]


heyButton : Model -> Component
heyButton model =
    menuButton [ onClick <| UmapSender model.umapParams ] [ text <| "Map" ]


downloadSvgButton : Component
downloadSvgButton =
    menuButton [ onClick GetSvg ] [ text "Download Svg" ]
