module View.DataWindow exposing (..)

import Array as A exposing (Array)
import Array.Extra as A
import Css
import Dict
import Draggable
import Html.Attributes
import Html.Events
import Html.Styled exposing (..)
import Html.Styled.Attributes as Att exposing (css)
import Html.Styled.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Decode exposing (Decoder)
import List as L
import Maybe exposing (withDefault)
import Maybe.Extra
import Model.Model exposing (ColumnParams, Model, Msg(..))
import Set
import Svg
import Util.Array
import Util.Cmap
import Util.Matrix exposing (Matrix)
import Util.Util
import View.Components
    exposing
        ( Builder
        , Component
        , forSmallWidth
        , onChange
        , theme
        )
import View.Graph


viewPanel : Model -> Component
viewPanel model =
    let
        headers =
            A.map .name model.columnParams

        labels =
            A.map
                (A.zip headers
                    >> A.foldl
                        (\( header, el2 ) a ->
                            if List.any ((==) header) model.plotParams.labelColumns then
                                a ++ " | " ++ el2

                            else
                                a
                        )
                        ""
                )
                model.records

        colorsidx =
            Maybe.andThen (\channel -> Util.Array.getIdx channel headers) model.plotParams.colorChannel

        colorColumn =
            Maybe.andThen (\idx -> Util.Matrix.getColumn idx "" model.records) colorsidx
    in
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.backgroundColor theme.mediumblue
            , Css.display Css.block
            , Css.margin (Css.px 10)
            , Css.borderRadius (Css.px 10)
            , forSmallWidth
                [ Css.height (Css.px 800)
                , Css.width (Css.pct 95)
                , Css.padding (Css.px 0)
                ]
            ]
        ]
        [ div
            [ css
                [ Css.height (Css.pct 100)
                , Css.display Css.block
                ]
            ]
            [ lazy graphMap
                { positions = model.positions
                , labels = labels
                , colors = Maybe.map Util.Cmap.translate colorColumn
                , center = model.center
                , zoom = model.zoom
                , baseSize = model.plotParams.baseSize
                }
            ]
        ]


graphMap :
    { positions : Maybe Matrix
    , baseSize : Float
    , labels : Array String
    , colors : Maybe (Array String)
    , center : ( Float, Float )
    , zoom : Float
    }
    -> Component
graphMap { positions, baseSize, labels, colors, center, zoom } =
    let
        ndata =
            withDefault 0 (Maybe.map A.length positions)

        data =
            withDefault (A.fromList []) positions

        labels_ =
            labels

        colors_ =
            withDefault (A.repeat (A.length data) "#000000") colors

        sizes_ =
            A.repeat (A.length data) baseSize
    in
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.display Css.block
            , Css.overflow Css.auto
            ]
        ]
        [ fromUnstyled <|
            Svg.svg
                ([ Html.Attributes.style "width" "100%"
                 , Html.Attributes.style "height" "100%"
                 , Html.Attributes.id "graph"
                 , handleZoom Zoom
                 , Draggable.mouseTrigger "my-element" DragMsg
                 ]
                    ++ Draggable.touchTriggers "my-element" DragMsg
                )
                [ View.Graph.graph
                    { center = center
                    , positions = data
                    , labels = labels_
                    , colors = colors_
                    , sizes = sizes_
                    , zoom = zoom
                    }
                ]
        ]


handleZoom : (Float -> msg) -> Svg.Attribute msg
handleZoom onZoom =
    let
        alwaysPreventDefaultAndStopPropagation msg =
            { message = msg, stopPropagation = True, preventDefault = True }

        zoomDecoder : Decoder msg
        zoomDecoder =
            Decode.float
                |> Decode.field "deltaY"
                |> Decode.map onZoom
    in
    Html.Events.custom
        "wheel"
    <|
        Decode.map alwaysPreventDefaultAndStopPropagation zoomDecoder
