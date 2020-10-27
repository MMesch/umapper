module View.SideBar.UmapTab exposing (..)

import Array as A exposing (Array)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Model.Model exposing (Model, Msg(..), UmapParams)
import View.Components
    exposing
        ( Builder
        , Component
        )
import View.SideBar.Style exposing (reusableInput, reusableTab)


umapTab : UmapParams -> Component
umapTab umapParams =
    reusableTab { title = "Umap Control" }
        [ div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.justifyContent Css.spaceBetween
                ]
            ]
            [ reusableInput
                [ Att.type_ "number"
                , Att.placeholder "minDist"
                , Att.step "0.01"
                , Att.value <| String.fromFloat umapParams.minDist
                , onInput (\x -> SetUmapParams { umapParams | minDist = withDefault 0.1 (String.toFloat x) })
                ]
                []
            , reusableInput
                [ Att.type_ "number"
                , Att.placeholder "spread"
                , Att.step "0.01"
                , Att.value <| String.fromFloat umapParams.spread
                , css [ Css.width (Css.pct 30) ]
                , onInput (\x -> SetUmapParams { umapParams | spread = withDefault 1.0 (String.toFloat x) })
                ]
                []
            , reusableInput
                [ Att.type_ "number"
                , Att.placeholder "nNeighbours"
                , Att.step "1"
                , Att.value <| String.fromInt umapParams.nNeighbors
                , css [ Css.width (Css.pct 30) ]
                , onInput (\x -> SetUmapParams { umapParams | nNeighbors = withDefault 15 (String.toInt x) })
                ]
                []
            ]
        ]
