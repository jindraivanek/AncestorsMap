module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open Fable.Core.JsInterop

open ReactLeaflet

let USE_ARROWS = true

let posOnLine fraction (x1,y1) (x2,y2) =
    let (x1,x2) = if x1 > x2 then (x2,x1) else (x1,x2)
    let (y1,y2) = if y1 > y2 then (y2,y1) else (y1,y2)
    x1 + fraction * (x2 - x1), y1 + fraction * (y2 - y1)

let arrowPolyLine (fromx, fromy) (tox, toy) : (float * float) list =
    let headlen = 0.003 // length of head in pixels
    let (arrowx, arrowy) = posOnLine 0.5 (fromx, fromy) (tox, toy)
    let dx = tox - fromx
    let dy = toy - fromy
    let angle: float = atan2 dy dx
    [
        (arrowx - headlen * cos(angle - System.Math.PI / 6.), arrowy - headlen * sin(angle - System.Math.PI / 6.))
        (arrowx, arrowy)
        (arrowx - headlen * cos(angle + System.Math.PI / 6.), arrowy - headlen * sin(angle + System.Math.PI / 6.))
    ]
 
type Marker = {
    Latitude: float
    Longitude: float
    Ident : string
    Title: string
    Weight: float
    Properties: string list
}

type PropertyDef =
    | EdgeProperty of PolylineProps
    | VectorProperty of CircleProps

let grey = "#848484"
let propertiesDef = Map.ofSeq [
    "edgeColorGrey", EdgeProperty (PolylineProps.Color grey)
    "nodeColorGrey", VectorProperty (CircleProps.Color grey)
]

let nl = System.Environment.NewLine
let tryInt (s: string) = let (r,x) = System.Int32.TryParse s in if r then Some x else None
let tryFloat (s: string) = let (r,x) = System.Double.TryParse s in if r then Some x else None
let split (sep: string) (s: string) = s.Split([|sep|], System.StringSplitOptions.None) |> Seq.map (fun s -> s.Trim()) |> Seq.toList
let lines (s: string) = split nl s
let columns (s: string) = s |> split "\t" //|> List.collect (split ",")

let parse s = s |> lines |> List.map columns

let mkData s =
    let defaultYear = 0
    let defaultGps = "50.0872, 14.4211"
    let xs = parse s
    let yMin = xs |> List.choose (fun x -> List.tryItem 2 x |> Option.bind tryInt) |> List.min
    let yMax = xs |> List.choose (fun x -> List.tryItem 2 x |> Option.bind tryInt) |> List.max
    let nodes = xs |> List.sortBy (fun x -> List.tryItem 2 x |> Option.bind tryInt) |> List.choose (fun x ->
        printfn "%A" x
        if List.length x < 6 then None else
        let lnglat = x.[1] |> (fun s -> if s.Trim() = "" then defaultGps else s.Trim()) |> split ","
        let lng = lnglat.[0]
        let lat = lnglat.[1]
        let year = x.[2] |> tryInt |> Option.defaultValue defaultYear
        let loc = x.[0]
        let name = x.[3]
        let weight = (float (year - yMin) / float (yMax - yMin))

        let title = sprintf "%s - %s - %i - %s %s" loc name year (List.tryItem 4 x |> Option.defaultValue "") (List.tryItem 5 x |> Option.defaultValue "")
        let ident = List.tryItem 6 x |> Option.map (fun y -> sprintf "%s - %s" name y) |> Option.defaultValue name // TODO: temp solution
        let defProp = if year = defaultYear then "nodeColorGrey" else ""
        let properties = (List.tryItem 7 x |> Option.defaultValue defProp) |> split ","
        Some { Latitude = float lat; Longitude = float lng; Ident = ident; Title = title; Weight = weight; Properties = properties }
    )
    nodes

// The model holds data that you want to keep track of while the application is running
type Page = Map | LoadData | LocationTree
type Msg =
    | SetPage of Page
    | SetRawData of string
    | LoadData
type Model = { Page: Page; RawData : string; Markers: Marker list; Edges: (Marker * Marker) list }

type GraphType =
    | Mermaid
    | GraphViz

let toGraph t model =
    let headerCode = match t with | Mermaid -> "graph TD" | GraphViz -> "digraph G {"
    let nodeCode node label tooltip level = 
        match t with 
        | Mermaid -> sprintf "%s(%s)" node label 
        | GraphViz -> sprintf "%s [label=\"%s\", tooltip=\"%s\", level=%i];" node label tooltip level
    let edgeCode x y = match t with | Mermaid -> sprintf "%s --> %s" x y | GraphViz -> sprintf "%s -> %s;" x y
    let footerCode = match t with | Mermaid -> "" | GraphViz -> "}"
    let nodes = model.Markers
    let edges = model.Edges
    let label n = n.Title |> lines |> Seq.head |> split " - " |> Seq.head
    let tooltip n = n.Title |> split nl
    let nodeLabels = 
        let e = edges |> Seq.collect (fun (x,y) -> [x;y])
        let m = e |> Seq.map label |> Seq.distinct |> Seq.mapi (fun i n -> n, sprintf "N%i" i) |> Map.ofSeq
        e |> Seq.map (fun n -> n, m.[label n]) |> Map.ofSeq
    let nodes = 
        nodeLabels |> Map.toList |> List.groupBy snd |> List.map (fun (l, g) -> 
            let g = g |> Seq.map fst
            let n = Seq.head g
            nodeCode l (label n |> lines |> Seq.head) (g |> Seq.collect tooltip |> Seq.distinct |> String.concat "\\n") (int <| n.Weight * 100.)) |> List.sort
    let edges = edges |> List.map (fun (x,y) -> edgeCode nodeLabels.[y] nodeLabels.[x])
    Seq.concat [[headerCode]; nodes; edges; [footerCode]] |> String.concat nl

// defines the initial state and initial command (= side-effect) of the application
let loadData model =
    let nodes = mkData model.RawData
    let merge g =
        let x = Seq.head g
        { Latitude = x.Latitude
          Longitude = x.Longitude
          Ident = ""
          Title = g |> Seq.map (fun x -> x.Title) |> String.concat nl
          Weight = g |> Seq.map (fun x -> x.Weight) |> Seq.min 
          Properties = g |> List.collect (fun x -> x.Properties) }
    let data =
        nodes
        |> List.groupBy (fun x -> x.Latitude, x.Longitude)
        |> List.map (fun (_ ,g) -> merge g)
    let edges =
        let singleEdges =
            nodes |> List.mapi (fun i x -> nodes |> List.mapi (fun j y -> (i,j), (x, y))) |> List.collect id |> List.filter (fun ((i,j),_) -> i < j)
            |> List.filter (fun (_, (x, y)) -> x.Ident = y.Ident && (x.Latitude, x.Longitude) <> (y.Latitude, y.Longitude))
            // from each node select only first edge by ordering (nodes are ordered by year)
            |> List.groupBy (fun ((i,_), _) -> i) |> List.map (fun (_, g) -> g |> Seq.minBy (fun ((_,j), _) -> j))
            |> List.map snd

        singleEdges 
        |> List.groupBy (fun (x,y) -> x.Latitude, x.Longitude, y.Latitude, y.Longitude)
        |> List.map (fun (_,g) ->
            let xs = g |> List.map fst
            let ys = g |> List.map snd
            merge xs, merge ys)
    { model with Markers = data |> Seq.toList; Edges = edges }

let init () : Model =
    let initialModel = { Page = Map; RawData = MarkersData.data; Markers = []; Edges = [] } |> loadData
    initialModel

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model =
    printfn "%A" currentModel
    match msg with
    | SetPage Map -> { currentModel with Page = Map }
    | SetPage Page.LoadData -> { currentModel with Page = Page.LoadData }
    | SetPage Page.LocationTree -> { currentModel with Page = Page.LocationTree }
    | SetRawData s -> { currentModel with RawData = s }
    | LoadData -> { loadData currentModel with Page = Map }

let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://github.com/giraffe-fsharp/Giraffe" ] [ str "Giraffe" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://mangelmaxime.github.io/Fulma" ] [ str "Fulma" ]
           ]

    p [ ]
        [ strong [] [ str "SAFE Template" ]
          str " powered by: "
          components ]

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view model dispatch =
    let mapView() =
        let mkColor (r,g,b) = sprintf "rgb(%i,%i,%i)" (int r) (int g) (int b)
        let colorGradient (step: float) (r1,g1,b1) (r2,g2,b2) =
            let linStep i x y = x + (i * (y-x))
            let r = (linStep step r1 r2, linStep step g1 g2, linStep step b1 b2)
            printfn "%A" (step, r)
            r
        let getColor weight = colorGradient weight (255.,0.,0.) (0.,0.,0.) |> mkColor
        let getTitle x = x |> lines |> List.map (fun l -> p [] [str l]) |> div []
        let markers =
            model.Markers
            |> Seq.map (fun x ->
                let extraProps = 
                    x.Properties 
                    |> List.choose (fun p -> Map.tryFind p propertiesDef) 
                    |> List.choose (function | VectorProperty p -> Some p | _ -> None)
                ReactLeaflet.circle ([
                    CircleProps.Custom ("center", (!^ (x.Longitude, x.Latitude):Leaflet.LatLngExpression))
                    CircleProps.Radius (float 200)
                    CircleProps.Color (getColor x.Weight)
                    CircleProps.Opacity 1.0
                    // MarkerProps.Title x.Title
                    ] @ extraProps) [ ReactLeaflet.tooltip [] [getTitle x.Title] ]
            )
        let edges = model.Edges |> List.collect (fun (x,y) ->
            let extraProps = 
                x.Properties @ y.Properties 
                |> List.choose (fun p -> Map.tryFind p propertiesDef) 
                |> List.choose (function | EdgeProperty p -> Some p | _ -> None)
            let line = 
                ReactLeaflet.polyline ([
                    PolylineProps.Positions !^ [|!^(x.Longitude, x.Latitude); !^(y.Longitude, y.Latitude)|]
                    PolylineProps.Color (getColor (max x.Weight y.Weight))
                    ] @ extraProps)
                    [ReactLeaflet.tooltip [] [div [] [getTitle x.Title; getTitle y.Title]]]
            let arrowHead() =
                ReactLeaflet.polyline ([
                    PolylineProps.Positions !^ (arrowPolyLine (x.Longitude, x.Latitude) (y.Longitude, y.Latitude) |> List.map (!^) |> List.toArray)
                    PolylineProps.Color (getColor (max x.Weight y.Weight))
                    ] @ extraProps) []
            line :: (if USE_ARROWS then [arrowHead()] else []))
        let avg xs = (Seq.sum xs) / float (Seq.length xs)
        let dist x y = (x.Longitude - y.Longitude)**2.0 + (x.Latitude - y.Latitude)**2.0 |> sqrt
        let sumDist xs y = Seq.sumBy (fun x -> dist x y) xs
        let center =
            let m = model.Markers |> Seq.minBy (sumDist model.Markers)
            m.Longitude, m.Latitude
        ReactLeaflet.map [
            MapProps.Center !^ center
            MapProps.SetView true
            MapProps.Zoom (float 12)
            MapProps.ZoomSnap 0.1
            MapProps.Id "myMap"
            MapProps.Style [ CSSProp.Height "650px" ]
        ] [
            yield tileLayer [
                TileLayerProps.Attribution "&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors"
                TileLayerProps.Url "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
            ] []
            yield! markers
            yield! edges
          ]


    let loadDataView() =
        Columns.columns [] [
            Column.column [] [
            str "Data format (delimeter TAB): location | GPS(lat,lng) | year | name | note | type"
            textarea [
                DefaultValue model.RawData
                OnChange (fun ev -> (!!ev.target?value) |> SetRawData |> dispatch)
                Rows 20
                Cols 120
            ] []
            ]
        ]

    let locationTreeView() =
        Columns.columns [] [
            Column.column [] [
            str "GraphViz graph format, copy paste to "
            a [ Href "https://dreampuf.github.io/GraphvizOnline" ] [ str "GraphvizOnline" ]
            textarea [
                DefaultValue (toGraph GraphViz model)
                Rows 20
                Cols 120
            ] []
            ]
        ]    

    div [] [
        Navbar.navbar [ Navbar.Color IsPrimary] [
            Navbar.Brand.div [] [
                div [] [ Heading.h2 [] [ str "Ancestors map" ] ]
            ]
            Navbar.Start.div [] [ Navbar.Item.div []
                (match model.Page with
                 | Page.LoadData ->
                    [ button "Apply" (fun _ -> dispatch (LoadData)) ]
                 | Page.LocationTree ->
                    [ button "Back" (fun _ -> dispatch (LoadData)) ]                
                 | Page.Map -> [ 
                     button "Load Data" (fun _ -> dispatch (SetPage Page.LoadData))
                     button "Location Tree" (fun _ -> dispatch (SetPage Page.LocationTree))
                     ] )
            ]
        ]

        (match model.Page with
         | Map -> mapView()
         | Page.LoadData -> loadDataView()
         | LocationTree -> locationTreeView())

        // Footer.footer [] [
        //     Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
        //         safeComponents
        //     ]
        // ]
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkSimple init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run

let main() = printfn "%s" MarkersData.data