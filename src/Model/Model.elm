port module Model.Model exposing (..)

import Array as A exposing (Array)
import Csv exposing (Csv, parse)
import File exposing (File)
import File.Download
import File.Select as Select
import Matrix exposing (Matrix)
import Maybe exposing (Maybe(..), withDefault)
import Result exposing (toMaybe)
import Table
import Task
import Util


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | SetQuery String
    | SetTableState Table.State
    | UpdateWeight Int String
    | SetUmapParams UmapParams
    | UmapSender UmapParams
    | UmapReceiver Matrix
    | GetSvg
    | GotSvg String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ receiver UmapReceiver, gotSvg GotSvg ]


type alias UmapParams =
    { minDist : Float
    , spread : Float
    , nNeighbors : Int
    }


port umap : ( Matrix, UmapParams ) -> Cmd msg


port receiver : (Matrix -> msg) -> Sub msg


port getSvg : String -> Cmd msg


port gotSvg : (String -> msg) -> Sub msg


type alias Model =
    { csv : Maybe String
    , query : String
    , headers : Array String
    , weights : Array Float
    , records : Array (Array String)
    , positions : Maybe Matrix
    , tableState : Table.State
    , umapParams : UmapParams
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv = Nothing
      , headers = A.fromList [ "name", "participants" ]
      , records =
            A.fromList
                [ A.fromList [ "1/12/2020", "Joe;Mat;Hugo" ]
                , A.fromList [ "2/12/2020", "Joe;Mat" ]
                , A.fromList [ "3/12/2020", "Hans;Joe;Hugo" ]
                , A.fromList [ "4/12/2020", "Mat;Hugo" ]
                , A.fromList [ "5/12/2020", "Mat;Hugo" ]
                , A.fromList [ "6/12/2020", "Mat;John" ]
                , A.fromList [ "7/12/2020", "Mat;John;Hugo" ]
                , A.fromList [ "8/12/2020", "Joe;Hugo" ]
                , A.fromList [ "9/12/2020", "John;Hugo" ]
                , A.fromList [ "10/12/2020", "Mat;Hugo" ]
                , A.fromList [ "11/12/2020", "Mat;Joe;John" ]
                , A.fromList [ "12/12/2020", "Mat;Hugo;John" ]
                , A.fromList [ "13/12/2020", "Joe;Hugo" ]
                , A.fromList [ "14/12/2020", "John;Joe" ]
                , A.fromList [ "15/12/2020", "Mat;Hugo" ]
                ]
      , weights = A.fromList [ 0.0, 1.0 ]
      , positions = Nothing
      , query = ""
      , tableState = Table.initialSort "name"
      , umapParams = { minDist = 0.1, spread = 1.0, nNeighbors = 3 }
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

        SetUmapParams params ->
            ( { model | umapParams = params }, Cmd.none )

        UmapSender params ->
            let
                data =
                    Util.compareColumns model.weights model.records
            in
            ( model, umap ( data, params ) )

        UmapReceiver matrix ->
            ( { model | positions = Just matrix }, Cmd.none )

        UpdateWeight index value ->
            let
                updater =
                    --Debug.log ("setting weight " ++ Debug.toString index ++ " to " ++ Debug.toString value) <|
                    A.set index (withDefault 0 (String.toFloat value))
            in
            ( { model | weights = updater model.weights }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        GetSvg ->
            ( model, getSvg "graph" )

        GotSvg output ->
            ( model, downloadSvg output )


downloadSvg : String -> Cmd msg
downloadSvg svgContent =
    File.Download.string "map.svg" "image/svg+xml" svgContent


updateCsvModel : String -> Model -> Model
updateCsvModel content model =
    let
        parsed =
            toMaybe (parse content)

        headers =
            withDefault model.headers <| Maybe.map (.headers >> A.fromList) parsed

        records =
            withDefault model.records <| Maybe.map (.records >> List.map A.fromList >> A.fromList) parsed
    in
    { model
        | csv = Just content
        , headers = headers
        , records = records
        , weights = A.repeat (A.length headers) 0
    }


fileSelection : Cmd Msg
fileSelection =
    Select.file [ "text/csv" ] CsvSelected


fileLoad : File -> Cmd Msg
fileLoad file =
    Task.perform CsvLoaded (File.toString file)


tableConfig : Array String -> Table.Config (Array String) Msg
tableConfig headers =
    Table.config
        { toId = withDefault "" << A.get 0
        , toMsg = SetTableState
        , columns =
            List.map2
                (\a b -> Table.stringColumn a (withDefault "NA" << A.get b))
                (A.toList headers)
                (List.range 0 (A.length headers))
        }
