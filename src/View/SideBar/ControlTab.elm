module View.SideBar.ControlTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy exposing (lazy3)
import Model.Model exposing (Model, Msg(..))
import View.Components exposing (Builder, Component, theme)
import View.SideBar.Style exposing (Layout(..), reusableButton, reusableTab)


controlTab : Component
controlTab =
    reusableTab { title = "Control", layout = RowLayout }
        [ heyButton
        , loadButton
        , downloadSvgButton
        ]


loadButton : Component
loadButton =
    reusableButton [ onClick CsvRequested, css [ Css.width (Css.pct 30) ] ] [ text "Load CSV" ]


heyButton : Component
heyButton =
    reusableButton [ onClick <| UmapSender, css [ Css.width (Css.pct 30) ] ] [ text <| "UMap" ]


downloadSvgButton : Component
downloadSvgButton =
    reusableButton [ onClick GetSvg, css [ Css.width (Css.pct 30) ] ] [ text "Get Svg" ]
