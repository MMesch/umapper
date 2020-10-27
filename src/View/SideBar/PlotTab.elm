module View.SideBar.PlotTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Model.Model exposing (Model, Msg(..), PlotParams)
import View.Components
    exposing
        ( Builder
        , Component
        )
import View.SideBar.Style exposing (Layout(..), reusableInput, reusableTab)


plotTab : PlotParams -> Component
plotTab plotParams =
    reusableTab { title = "Plot Control", layout = RowLayout }
        [ div [] [] ]
