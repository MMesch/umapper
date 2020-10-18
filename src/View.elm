module View exposing (..)

import Array
import Css
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Maybe exposing (withDefault)
import Model exposing (Model, Msg(..), tableConfig)
import Table


theme =
    { lightblue = Css.hex "5c94eb"
    , darkblue = Css.hex "2b456e"
    , mediumblue = Css.hex "5284d2"
    }


type alias Component =
    Html Msg


type alias Builder =
    List (Attribute Msg) -> List (Html Msg) -> Html Msg


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        mainWindow <|
            [ dataPanel [ loadButton ], viewPanel model ]


loadButton : Component
loadButton =
    button [ onClick CsvRequested ] [ text "Load CSV" ]


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


viewPanel : Model -> Component
viewPanel model =
    panel
        [ css
            [ Css.whiteSpace Css.pre
            , Css.width (Css.vw 70)
            , Css.height (Css.vh 90)
            , Css.overflow Css.scroll
            ]
        ]
        [ fromUnstyled
            (Table.view
                (tableConfig model)
                model.tableState
                (withDefault [] <| Maybe.map Array.toList model.records)
            )
        ]


dataPanel : List Component -> Component
dataPanel =
    panel
        [ css
            [ Css.width (Css.vw 20)
            , Css.height (Css.vh 90)
            ]
        ]
