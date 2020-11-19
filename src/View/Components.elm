module View.Components exposing (..)

import Css
import Css.Media
import Html.Styled exposing (..)
import Html.Styled.Events exposing (on)
import Json.Decode as D
import Model.Model exposing (Msg)


forSmallWidth =
    Css.Media.withMedia
        [ Css.Media.only Css.Media.screen
            [ Css.Media.maxWidth (Css.px 1000) ]
        ]


forLargeWidth =
    Css.Media.withMedia
        [ Css.Media.only Css.Media.screen
            [ Css.Media.minWidth (Css.px 1000) ]
        ]


type alias Component =
    Html Msg


type alias Builder =
    List (Attribute Msg) -> List (Html Msg) -> Html Msg


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" <| D.map handler <| D.at [ "target", "value" ] D.string


theme =
    { light = Css.hex "f2f1ee"
    , light2 = Css.hex "f0dba0"
    , dark = Css.hex "292b34"
    , medium = Css.hex "e0dfd8"
    , white = Css.hex "ffffff"
    , black = Css.hex "000000"
    }
