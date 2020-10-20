port module Model exposing (..)

import Array
import Csv exposing (Csv, parse)
import File exposing (File)
import File.Select as Select
import Maybe exposing (Maybe(..), withDefault)
import Result exposing (toMaybe)
import Table
import Task


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | SetQuery String
    | SetTableState Table.State
    | UpdateWeight Int String
    | SayHello


port umap : Array.Array (Array.Array Float) -> Cmd msg


type alias Model =
    { csv : Maybe String
    , query : String
    , headers : Maybe (Array.Array String)
    , weights : Maybe (Array.Array Float)
    , records : Maybe (Array.Array (Array.Array String))
    , tableState : Table.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv = Nothing
      , headers = Nothing
      , records = Nothing
      , weights = Nothing
      , query = ""
      , tableState = Table.initialSort "name"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( model, fileSelection )

        CsvSelected file ->
            ( model, fileLoad file )

        CsvLoaded content ->
            ( updateCsvModel content model, Cmd.none )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SayHello ->
            ( model, sendMessage () )

        UpdateWeight index value ->
            let
                updater =
                    Array.set index (withDefault 0 (String.toFloat value))
            in
            ( { model | weights = Maybe.map updater model.weights }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


updateCsvModel : String -> Model -> Model
updateCsvModel content model =
    let
        parsed =
            toMaybe (parse content)

        headers =
            Maybe.map (.headers >> Array.fromList) parsed
    in
    { model
        | csv = Just content
        , headers = headers
        , records =
            Maybe.map
                (.records
                    >> List.map Array.fromList
                    >> Array.fromList
                )
                parsed
        , weights =
            case headers of
                Nothing ->
                    Nothing

                Just headers_ ->
                    Just (Array.repeat (Array.length headers_) 0)
    }


fileSelection : Cmd Msg
fileSelection =
    Select.file [ "text/csv" ] CsvSelected


fileLoad : File -> Cmd Msg
fileLoad file =
    Task.perform CsvLoaded (File.toString file)


tableConfig : Model -> Table.Config (Array.Array String) Msg
tableConfig model =
    Table.config
        { toId = withDefault "" << Array.get 0
        , toMsg = SetTableState
        , columns =
            case model.headers of
                Just headers ->
                    List.map2
                        (\a b -> Table.stringColumn a (withDefault "NA" << Array.get b))
                        (Array.toList headers)
                        (List.range 0 (Array.length headers))

                Nothing ->
                    []
        }
