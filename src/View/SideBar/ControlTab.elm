module View.SideBar.ControlTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Lazy exposing (lazy3)
import Model.Model exposing (Model, Msg(..))
import View.Components exposing (Builder, Component, theme)
import View.SideBar.Style exposing (reusableButton, reusableTab)


controlTab : Component
controlTab =
    reusableTab { title = "Control" }
        [ heyButton
        , loadButton
        , downloadSvgButton
        ]


loadButton : Component
loadButton =
    reusableButton [ onClick CsvRequested ] [ text "Load CSV" ]


heyButton : Component
heyButton =
    reusableButton [ onClick <| UmapSender ] [ text <| "Map" ]


downloadSvgButton : Component
downloadSvgButton =
    reusableButton [ onClick GetSvg ] [ text "Download Svg" ]
