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
        , theme
        )
import View.SideBar.Style exposing (Layout(..), reusableInput, reusableTab)


umapTab : UmapParams -> Component
umapTab umapParams =
    reusableTab { title = "Umap", layout = RowLayout }
        [ reusableInput (Just "minimum Distance")
            [ Att.type_ "number"
            , Att.placeholder "minDist"
            , Att.step "0.01"
            , Att.value <| String.fromFloat umapParams.minDist
            , onInput (\x -> SetUmapParams { umapParams | minDist = withDefault 0.1 (String.toFloat x) })
            ]
        , reusableInput (Just "spread")
            [ Att.type_ "number"
            , Att.placeholder "spread"
            , Att.step "0.01"
            , Att.value <| String.fromFloat umapParams.spread
            , onInput (\x -> SetUmapParams { umapParams | spread = withDefault 1.0 (String.toFloat x) })
            ]
        , reusableInput (Just "nNeighbors")
            [ Att.type_ "number"
            , Att.placeholder "nNeighbours"
            , Att.step "1"
            , Att.value <| String.fromInt umapParams.nNeighbors
            , onInput (\x -> SetUmapParams { umapParams | nNeighbors = withDefault 15 (String.toInt x) })
            ]
        ]
