module Main exposing (main)

import Browser
import Model.Model exposing (Model, Msg, init, subscriptions, update)
import Task
import View.View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
