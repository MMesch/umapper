module View.SideBar.Style exposing (..)

import Array as A exposing (Array)
import Css exposing (row)
import Dropdown
import Html.Attributes as UnstyledAtt
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onInput)
import Model.Model exposing (Msg)
import MultiSelect
import View.Components exposing (Builder, Component, theme)


buttonStyle =
    [ Css.borderRadius (Css.px 2)
    , Css.backgroundColor theme.lightblue
    , Css.color theme.black
    , Css.border (Css.px 0)
    , Css.fontSize (Css.rem 1.5)
    , Css.display Css.inlineBlock
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
            , Css.flexGrow (Css.num 1)
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


reusableInput : Maybe String -> List (Attribute Msg) -> Component
reusableInput name atts =
    let
        inputElem =
            input
                ([ css
                    [ Css.width (Css.px 80)
                    , Css.fontSize (Css.rem 1.2)
                    , Css.display Css.inlineBlock
                    ]
                 ]
                    ++ atts
                )
                []
    in
    case name of
        Just name_ ->
            inputWrapper name_ inputElem

        Nothing ->
            inputElem


inputWrapper : String -> Component -> Component
inputWrapper title c =
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
                , Css.width (Css.px 110)
                , Css.display Css.inlineBlock
                , Css.textAlign Css.center
                ]
            ]
            [ text title ]
        , c
        ]


reusableSelect :
    (Maybe String -> Msg)
    ->
        { title : Maybe String
        , values : List String
        , hasEmpty : Bool
        , selected : Maybe String
        }
    -> Component
reusableSelect msg { title, values, hasEmpty, selected } =
    let
        dropdown =
            fromUnstyled <|
                Dropdown.dropdown
                    { items = List.map (\v -> { value = v, text = v, enabled = True }) values
                    , emptyItem =
                        if hasEmpty then
                            Just <| { value = "nothing", text = "nothing", enabled = True }

                        else
                            Nothing
                    , onChange = msg
                    }
                    (List.map (\( a, b ) -> UnstyledAtt.style a b)
                        [ ( "display", "inline-block" )
                        , ( "width", "80px" )
                        ]
                    )
                    selected
    in
    case title of
        Just title_ ->
            inputWrapper title_ dropdown

        Nothing ->
            dropdown


reusableMultiSelect :
    (List String -> Msg)
    ->
        { title : Maybe String
        , values : List String
        , hasEmpty : Bool
        , selected : List String
        }
    -> Component
reusableMultiSelect msg { title, values, hasEmpty, selected } =
    let
        multiselect =
            fromUnstyled <|
                MultiSelect.multiSelect
                    { items =
                        List.map
                            (\h ->
                                { value = h
                                , text = h
                                , enabled = True
                                }
                            )
                            values
                    , onChange = msg
                    }
                    []
                    selected
    in
    case title of
        Just title_ ->
            inputWrapper title_ multiselect

        Nothing ->
            multiselect
