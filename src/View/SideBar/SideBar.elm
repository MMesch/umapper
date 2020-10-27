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
        , panel
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
            [ Css.margin (Css.px 5)
            , Css.height (Css.pct 100)
            , Css.display Css.block
            , forLargeWidth
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
            [ controlTab
            , lazy3 tableTab model.tableState (A.map .name model.columnParams) model.records
            , lazy umapTab model.umapParams
            , lazy similarityTab model.columnParams
            , lazy plotTab model.plotParams
            ]
        ]
