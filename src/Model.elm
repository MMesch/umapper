module Model exposing (..)

import Array
import Csv exposing (Csv, parse)
import File exposing (File)
import File.Select as Select
import Maybe exposing (withDefault)
import Result exposing (toMaybe)
import Table
import Task


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | SetQuery String
    | SetTableState Table.State


type alias Model =
    { csv : Maybe String
    , query : String
    , headers : Maybe (Array.Array String)
    , records : Maybe (Array.Array (Array.Array String))
    , tableState : Table.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv = Nothing
      , headers = Nothing
      , records = Nothing
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
            let
                parsed =
                    toMaybe (parse content)
            in
            ( { model
                | csv = Just content
                , headers = Maybe.map (.headers >> Array.fromList) parsed
                , records =
                    Maybe.map
                        (.records
                            >> List.map Array.fromList
                            >> Array.fromList
                        )
                        parsed
              }
            , Cmd.none
            )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


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
