module View.View exposing (..)

import Css
import Css.Media
import Html
import Html.Styled exposing (div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Lazy exposing (lazy)
import Model.Model exposing (Model, Msg(..))
import View.Components exposing (Component, forSmallWidth, theme)
import View.DataWindow exposing (viewPanel)
import View.SideBar.SideBar exposing (sidebar)


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        mainWindow <|
            [ sidebar model
            , viewPanel model
            ]


mainWindow : List Component -> Component
mainWindow components =
    div
        [ css
            [ Css.backgroundColor theme.lightblue
            , Css.minHeight (Css.vh 100)
            , Css.width (Css.vw 100)
            ]
        ]
        [ div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , forSmallWidth
                    [ Css.flexDirection Css.column
                    , Css.padding (Css.px 3)
                    ]
                ]
            ]
            components
        ]
