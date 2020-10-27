module View.SideBar.TableTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Model.Model exposing (Model, Msg)
import Table
import View.Components exposing (Component, theme)
import View.SideBar.Style exposing (reusableTab)


tableTab : Table.State -> Array String -> Array (Array String) -> Component
tableTab tableState headers records =
    reusableTab { title = "Data Table" }
        [ div
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
            [ table headers tableState records ]
        ]


table : Array String -> Table.State -> Array (Array String) -> Html Msg
table headers tableState records =
    fromUnstyled
        (Table.view
            (Model.Model.tableConfig headers)
            tableState
            (A.toList records)
        )
