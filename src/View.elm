module View exposing (..)

import Array
import Array.Extra as Array
import Css
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Model exposing (Model, Msg(..), tableConfig)
import Table


theme =
    { lightblue = Css.hex "5c94eb"
    , darkblue = Css.hex "2b456e"
    , mediumblue = Css.hex "5284d2"
    , white = Css.hex "ffffff"
    }


type alias Component =
    Html Msg


type alias Builder =
    List (Attribute Msg) -> List (Html Msg) -> Html Msg


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        mainWindow <|
            [ dataPanel model, viewPanel model ]


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


panel : Builder
panel =
    styled
        div
        [ Css.backgroundColor theme.mediumblue
        , Css.whiteSpace Css.pre
        , Css.display Css.block
        , Css.margin (Css.px 10)
        , Css.padding (Css.px 10)
        , Css.borderRadius (Css.px 10)
        ]


namedSlider : Int -> Float -> Component
namedSlider index value =
    div []
        [ input
            [ A.type_ "range"
            , A.min "0"
            , A.max "10"
            , A.value <| String.fromFloat value
            , onInput (UpdateWeight index)
            ]
            [ text <| String.fromFloat value ]
        ]


viewPanel : Model -> Component
viewPanel model =
    panel
        [ css
            [ Css.whiteSpace Css.pre
            , Css.width (Css.vw 100)
            , Css.height (Css.vh 90)
            , Css.overflow Css.auto
            ]
        ]
        (let
            builder a b =
                Array.toList <| Array.map2 namedSlider (Array.fromList (List.range 0 (Array.length a))) b
         in
         withDefault [] <| Maybe.map2 builder model.headers model.weights
        )


dataTable : Model -> Component
dataTable model =
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.overflow Css.auto
            , Css.padding (Css.px 5)
            ]
        ]
        [ fromUnstyled
            (Table.view
                (tableConfig model)
                model.tableState
                (withDefault [] <| Maybe.map Array.toList model.records)
            )
        ]


dataPanel : Model -> Component
dataPanel model =
    panel
        [ css
            [ Css.width (Css.vw 20)
            , Css.height (Css.vh 90)
            , Css.displayFlex
            , Css.flexDirection Css.row
            ]
        ]
        [ heyButton, loadButton, columnSelector, dataTable model ]


menuButton : Builder
menuButton =
    styled
        button
        [ Css.borderRadius (Css.px 2)
        , Css.backgroundColor theme.darkblue
        , Css.color theme.white
        , Css.border (Css.px 0)
        , Css.width (Css.pct 100)
        , Css.display Css.block
        , Css.margin (Css.px 2)
        ]


loadButton : Component
loadButton =
    menuButton [ onClick CsvRequested ] [ text "Load CSV" ]


heyButton : Component
heyButton =
    menuButton [ onClick SayHello ] [ text "Say Hello" ]


columnSelector : Component
columnSelector =
    menuButton [ onClick CsvRequested ] [ text "Select Columns" ]
