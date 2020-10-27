module View.SideBar.Style exposing (..)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import View.Components exposing (Builder, Component, theme)


buttonStyle =
    [ Css.borderRadius (Css.px 2)
    , Css.backgroundColor theme.lightblue
    , Css.color theme.black
    , Css.border (Css.px 0)
    , Css.fontSize (Css.rem 1.5)
    , Css.width (Css.pct 95)
    , Css.display Css.block
    , Css.marginTop (Css.px 2)
    , Css.marginBottom (Css.px 2)
    ]


reusableButton : Builder
reusableButton =
    styled button buttonStyle


tabStyle =
    [ Css.borderRadius (Css.px 5)
    , Css.width (Css.pct 100)
    , Css.display Css.block
    , Css.backgroundColor theme.darkblue
    , Css.marginTop (Css.px 2)
    , Css.padding (Css.px 10)
    ]


type Layout
    = RowLayout
    | ColumnLayout


layout : Layout -> List Css.Style
layout x =
    case x of
        RowLayout ->
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.flexWrap Css.wrap
            ]

        ColumnLayout ->
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            ]


type alias TabParams =
    { title : String
    , layout : Layout
    }


reusableTab : TabParams -> List Component -> Component
reusableTab params components =
    div [ css tabStyle ]
        [ div
            [ css
                [ Css.color theme.white
                , Css.margin (Css.px 5)
                ]
            ]
            [ text params.title ]
        , div
            [ css (layout params.layout) ]
            components
        ]


inputStyle =
    [ Css.width (Css.pct 30), Css.fontSize (Css.rem 1.2) ]


reusableInput : Builder
reusableInput =
    styled input inputStyle
