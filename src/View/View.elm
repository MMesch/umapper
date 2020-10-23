module View.View exposing (..)

import Css
import Html
import Html.Styled exposing (div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Lazy exposing (lazy)
import Model.Model exposing (Model, Msg(..))
import View.Components exposing (Component, theme)
import View.DataView exposing (viewPanel)
import View.SideBar exposing (dataPanel)


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        mainWindow <|
            [ dataPanel model
            , viewPanel model
            ]


mainWindow : List Component -> Component
mainWindow components =
    div
        [ css
            [ Css.backgroundColor theme.lightblue
            , Css.minHeight (Css.vh 100)
            , Css.minWidth (Css.vw 100)
            ]
        ]
        [ div
            [ css
                [ Css.padding (Css.px 20)
                , Css.displayFlex
                , Css.flexDirection Css.row
                ]
            ]
            components
        ]
