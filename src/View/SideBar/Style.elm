module View.SideBar.Style exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onInput)
import Model.Model exposing (Msg)
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
    | GridLayout Int Int


layout : Layout -> List Css.Style
layout x =
    case x of
        RowLayout ->
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            , Css.flexWrap Css.wrap
            ]

        ColumnLayout ->
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            ]

        GridLayout i1 i2 ->
            [ Css.property "display" "grid"
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


reusableInput : String -> List (Attribute Msg) -> Component
reusableInput name atts =
    div
        [ css
            [ Css.width (Css.px 200)
            , Css.margin (Css.px 10)
            , Css.displayFlex
            , Css.alignItems Css.center
            ]
        ]
        [ label
            [ css
                [ Css.color theme.white
                , Css.display Css.inlineBlock
                , Css.width (Css.px 110)
                , Css.textAlign Css.center
                ]
            ]
            [ text name ]
        , input
            ([ css
                [ Css.width (Css.px 80)
                , Css.fontSize (Css.rem 1.2)
                , Css.display Css.inlineBlock
                ]
             ]
                ++ atts
            )
            []
        ]


reusableSelect :
    (String -> Msg)
    -> { title : String, values : List String, selected : String }
    -> Component
reusableSelect msg { title, values, selected } =
    div [ css [ Css.width (Css.px 200), Css.margin (Css.px 10) ] ]
        [ label
            [ css
                [ Css.color theme.white
                , Css.width (Css.px 110)
                , Css.display Css.inlineBlock
                , Css.textAlign Css.center
                ]
            ]
            [ text title ]
        , select
            [ onInput msg
            , css
                [ Css.display Css.inlineBlock
                , Css.width (Css.px 80)
                ]
            ]
          <|
            List.map
                (\value ->
                    option
                        ([ Att.value value ]
                            ++ (if value == selected then
                                    [ Att.selected True ]

                                else
                                    []
                               )
                        )
                        [ text value ]
                )
                values
        ]
