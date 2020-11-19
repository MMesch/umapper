module View.SideBar.SideBar exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy exposing (lazy, lazy2, lazy3)
import Maybe exposing (withDefault)
import Model.Model exposing (Model, Msg(..))
import View.Components
    exposing
        ( Builder
        , Component
        , forLargeWidth
        , forSmallWidth
        , onChange
        , theme
        )
import View.SideBar.ControlTab exposing (controlTab)
import View.SideBar.PlotTab exposing (plotTab)
import View.SideBar.SimilarityTab exposing (similarityTab)
import View.SideBar.TableTab exposing (tableTab)
import View.SideBar.UmapTab exposing (umapTab)


sidebar : Model -> Component
sidebar model =
    div
        [ css
            [ Css.display Css.block
            , forLargeWidth
                [ Css.width (Css.pct 40)
                , Css.maxWidth (Css.px 600)
                , Css.overflowY Css.auto
                , Css.maxHeight (Css.vh 90)
                , Css.margin (Css.px 10)
                ]
            , Css.pseudoElement "-webkit-scrollbar"
                [ Css.width (Css.px 5)
                ]
            ]
        ]
        [ div
            [ css
                [ Css.width (Css.pct 93)
                , Css.marginLeft (Css.px 20)
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                ]
            ]
            (let
                headers =
                    A.map .name model.columnParams
             in
             [ lazy3 tableTab model.tableState headers model.records
             , lazy umapTab model.umapParams
             , lazy similarityTab model.columnParams
             , lazy2 plotTab model.plotParams headers
             ]
            )
        ]
