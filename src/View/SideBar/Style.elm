module View.SideBar.Style exposing (..)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import View.Components exposing (Builder, Component, theme)


buttonStyle =
    [ Css.borderRadius (Css.px 2)
    , Css.backgroundColor theme.darkblue
    , Css.color theme.white
    , Css.border (Css.px 0)
    , Css.width (Css.pct 100)
    , Css.display Css.block
    , Css.margin (Css.px 2)
    ]


reusableButton : Builder
reusableButton =
    styled button buttonStyle


tabStyle =
    [ Css.borderRadius (Css.px 2)
    , Css.width (Css.pct 100)
    , Css.display Css.block
    , Css.backgroundColor theme.mediumblue
    ]


type alias TabParams =
    { title : String }


reusableTab : TabParams -> List Component -> Component
reusableTab params components =
    div [ css tabStyle ] ([ h3 [] [ text params.title ] ] ++ components)


inputStyle =
    [ Css.width (Css.pct 30) ]


reusableInput : Builder
reusableInput =
    styled input inputStyle
