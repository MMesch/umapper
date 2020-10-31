module View.SideBar.PlotTab exposing (..)

import Array as A exposing (Array)
import Css
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
import View.SideBar.Style exposing (Layout(..), reusableInput, reusableSelect, reusableTab)


plotTab : PlotParams -> Array String -> Component
plotTab params headers =
    reusableTab { title = "Plot Control", layout = RowLayout }
        [ reusableSelect SetChannelColor
            { title = "color channel"
            , selected = withDefault "nothing" params.colorChannel
            , values = "nothing" :: A.toList headers
            }
        , reusableSelect SetChannelSize
            { title = "size channel"
            , selected = withDefault "nothing" params.sizeChannel
            , values = "nothing" :: A.toList headers
            }
        , fromUnstyled <|
            MultiSelect.multiSelect
                { items = List.map (\h -> { value = h, text = h, enabled = True }) <| A.toList headers
                , onChange = SetLabelColumns
                }
                []
                (A.toList headers)
        ]
