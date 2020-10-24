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
    { lightblue = Css.hex "5c94eb"
    , darkblue = Css.hex "2b456e"
    , mediumblue = Css.hex "5284d2"
    , white = Css.hex "ffffff"
    }


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
