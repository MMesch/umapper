module View.SideBar.PlotTab exposing (..)

import Array as A exposing (Array)
import Css
import Dropdown
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Model.Model exposing (Model, Msg(..), PlotParams)
import MultiSelect
import Util.Cmap
import View.Components
    exposing
        ( Builder
        , Component
        )
import View.SideBar.Style
    exposing
        ( Layout(..)
        , reusableInput
        , reusableMultiSelect
        , reusableSelect
        , reusableTab
        )


plotTab : PlotParams -> Array String -> Component
plotTab params headers =
    reusableTab { title = "Plot Control", layout = RowLayout }
        [ reusableSelect (\x -> SetPlotParams { params | colorChannel = x })
            { title = "color channel"
            , hasEmpty = True
            , selected = params.colorChannel
            , values = A.toList headers
            }
        , reusableSelect (\x -> SetPlotParams { params | sizeChannel = x })
            { title = "size channel"
            , hasEmpty = True
            , selected = params.sizeChannel
            , values = A.toList headers
            }
        , reusableMultiSelect (\x -> SetPlotParams { params | labelColumns = x })
            { title = "label columns"
            , hasEmpty = False
            , values = A.toList headers
            , selected = params.labelColumns
            }
        , reusableInput "base size"
            [ Att.type_ "number"
            , Att.placeholder "spread"
            , Att.step "0.01"
            , Att.value <| String.fromFloat params.baseSize
            , onInput (\x -> SetPlotParams { params | baseSize = withDefault 1.0 (String.toFloat x) })
            ]
        ]
