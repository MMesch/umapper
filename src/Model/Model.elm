port module Model.Model exposing (..)

import Array as A exposing (Array)
import Css exposing (position)
import Csv exposing (Csv, parse)
import Draggable exposing (Msg)
import File exposing (File)
import File.Download
import File.Select as Select
import Maybe exposing (Maybe(..), withDefault)
import Result exposing (toMaybe)
import String exposing (String)
import Table
import Task
import Util.Cmap as Cmap exposing (Colormap)
import Util.Matrix exposing (Matrix)
import Util.Util


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | SetQuery String
    | SetTableState Table.State
    | SetUmapParams UmapParams
    | SetPlotParams PlotParams
    | SetColumnWeight Int String
    | SetColumnDistance Int (Maybe String)
    | UmapSender
    | UmapReceiver Matrix
    | GetSvg
    | GotSvg String
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | Zoom Float


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Sub.batch
        [ receiver UmapReceiver
        , gotSvg GotSvg
        , Draggable.subscriptions DragMsg drag
        ]


type alias UmapParams =
    { minDist : Float
    , spread : Float
    , nNeighbors : Int
    }


port umap : ( Matrix, UmapParams ) -> Cmd msg


port receiver : (Matrix -> msg) -> Sub msg


port getSvg : String -> Cmd msg


port gotSvg : (String -> msg) -> Sub msg


type DistanceFunction
    = MultiString
    | Euclidean
    | NumberOfEntries


distanceMap : List ( String, DistanceFunction )
distanceMap =
    [ ( "MultiString", MultiString )
    , ( "Euclidean", Euclidean )
    , ( "NumberOfEntries", NumberOfEntries )
    ]


toString : DistanceFunction -> String
toString =
    Util.Util.typeMapToString distanceMap


type alias ColumnParams =
    { weight : Float
    , name : String
    , distance : DistanceFunction
    }


defaultColumnParams : ColumnParams
defaultColumnParams =
    { name = "no name"
    , weight = 0
    , distance = MultiString
    }


type alias PlotParams =
    { labelColumns : List String
    , colorChannel : Maybe String
    , sizeChannel : Maybe String
    , baseSize : Float
    , colorMap : Colormap
    }


defaultPlotParams : PlotParams
defaultPlotParams =
    { labelColumns = []
    , colorChannel = Nothing
    , sizeChannel = Nothing
    , baseSize = 1
    , colorMap = Cmap.Quantitative
    }


type alias Model =
    { query : String
    , columnParams : Array ColumnParams
    , plotParams : PlotParams
    , records : Array (Array String)
    , positions : Maybe Matrix
    , tableState : Table.State
    , umapParams : UmapParams
    , center : ( Float, Float )
    , drag : Draggable.State String
    , zoom : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { records =
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
      , columnParams =
            A.fromList
                [ { name = "date", weight = 0.0, distance = MultiString }
                , { name = "participants", weight = 1.0, distance = MultiString }
                ]
      , positions = Nothing
      , query = ""
      , tableState = Table.initialSort "name"
      , umapParams = { minDist = 0.1, spread = 1.0, nNeighbors = 3 }
      , plotParams = { defaultPlotParams | labelColumns = [ "date", "participants" ] }
      , center = ( 0, 0 )
      , drag = Draggable.init
      , zoom = 1.0
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

        UmapSender ->
            let
                weights =
                    A.map .weight model.columnParams

                data =
                    Util.Util.compareColumns weights model.records
            in
            ( model, umap ( data, model.umapParams ) )

        UmapReceiver matrix ->
            ( { model | positions = Just matrix }, Cmd.none )

        SetColumnWeight index value ->
            let
                oldColumnParams =
                    withDefault defaultColumnParams
                        (A.get index model.columnParams)

                newColumnParams =
                    --Debug.log ("setting weight " ++ Debug.toString index ++ " to " ++ Debug.toString value) <|
                    { oldColumnParams | weight = withDefault 0 (String.toFloat value) }
            in
            ( { model | columnParams = A.set index newColumnParams model.columnParams }
            , Cmd.none
            )

        SetColumnDistance index value ->
            ( model, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        GetSvg ->
            ( model, getSvg "graph" )

        GotSvg output ->
            ( model, downloadSvg output )

        OnDragBy ( dx, dy ) ->
            let
                ( x, y ) =
                    model.center
            in
            ( { model
                | center =
                    ( x - 0.1 * dx
                    , y - 0.1 * dy
                    )
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        Zoom factor ->
            let
                newZoom =
                    model.zoom
                        |> (+) (factor * 0.005)
                        |> clamp 0.5 5
            in
            ( { model | zoom = newZoom }, Cmd.none )

        SetPlotParams params ->
            ( { model | plotParams = params }, Cmd.none )


downloadSvg : String -> Cmd msg
downloadSvg svgContent =
    File.Download.string "map.svg" "image/svg+xml" svgContent


updateCsvModel : String -> Model -> Model
updateCsvModel content model =
    case toMaybe (parse content) of
        Just parsed ->
            let
                columnParams =
                    A.fromList <| List.map (\x -> { defaultColumnParams | name = x }) parsed.headers

                records =
                    (.records >> List.map A.fromList >> A.fromList) parsed
            in
            { model
                | columnParams = columnParams
                , records = records
            }

        Nothing ->
            model


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
