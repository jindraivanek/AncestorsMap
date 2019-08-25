module Client

open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fulma
open ReactLeaflet

type Marker = {
    Latitude: float
    Longitude: float
    Ident : string
    Title: string
    Weight: float
}

let nl = System.Environment.NewLine
let tryInt (s: string) = let (r,x) = System.Int32.TryParse s in if r then Some x else None
let tryFloat (s: string) = let (r,x) = System.Double.TryParse s in if r then Some x else None
let split (sep: string) (s: string) = s.Split(sep.ToCharArray()) |> Seq.map (fun s -> s.Trim()) |> Seq.toList
let lines (s: string) = split nl s
let columns (s: string) = s |> split "\t" |> List.collect (split ",")

let parse s = s |> lines |> List.map columns

let mkData s =
    let xs = parse s
    let yMin = xs |> List.choose (fun x -> x.[3] |> tryInt) |> List.min
    let yMax = xs |> List.choose (fun x -> x.[3] |> tryInt) |> List.max
    let nodes = xs |> List.sortBy (fun x -> x.[3] |> tryInt) |> List.map (fun x ->
        let lng = x.[1]
        let lat = x.[2]
        let year = x.[3] |> int
        let loc = x.[0]
        let name = x.[4]
        let weight = (float (year - yMin) / float (yMax - yMin))
        
        let title = sprintf "%s - %s - %i" loc name year
        let ident = sprintf "%s - %i" name year
        { Latitude = float lat; Longitude = float lng; Ident = ident; Title = title; Weight = weight }
    ) 
    nodes 

// The model holds data that you want to keep track of while the application is running

type Msg = | Nop
type Model = { Markers: Marker list; Edges: (Marker * Marker) list }

// defines the initial state and initial command (= side-effect) of the application
let init () : Model =
    let nodes = mkData MarkersData.data
    let data = 
        nodes
        |> List.groupBy (fun x -> x.Latitude, x.Longitude)
        |> List.map (fun ((lat, lng) ,g) -> 
            { Latitude = lat; Longitude = lng; Ident = ""; Title = g |> Seq.map (fun x -> x.Title) |> String.concat nl; Weight = g |> Seq.map (fun x -> x.Weight) |> Seq.min })
    let edges = 
        nodes |> List.mapi (fun i x -> nodes |> List.mapi (fun j y -> (i,j), (x, y))) |> List.collect id |> List.filter (fun ((i,j),_) -> i < j) |> List.map snd
        |> List.filter (fun (x, y) -> x.Ident = y.Ident)
    let initialModel = { Markers = data |> Seq.toList; Edges = edges }

    initialModel

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model =
    currentModel

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
            ReactLeaflet.circle [
                CircleProps.Custom ("center", (!^ (x.Longitude, x.Latitude):Leaflet.LatLngExpression))
                CircleProps.Radius (float 200)
                CircleProps.Color (getColor x.Weight)
                CircleProps.Opacity 1.0
                // MarkerProps.Title x.Title
                ] [ ReactLeaflet.tooltip [] [getTitle x.Title] ]
        )
    let edges = model.Edges |> List.map (fun (x,y) -> 
        ReactLeaflet.polyline [
            PolylineProps.Positions !^ [|!^(x.Longitude, x.Latitude); !^(y.Longitude, y.Latitude)|]
            PolylineProps.Color (getColor (max x.Weight y.Weight))
            ] 
            [ReactLeaflet.tooltip [] [div [] [getTitle x.Title; getTitle y.Title]]])
    
    div [] [
        Navbar.navbar [ Navbar.Color IsPrimary ] [
            Navbar.Item.div [] [
                Heading.h2 [] [
                    str "Ancestors map"
                ]
            ]
        ]

        ReactLeaflet.map [
            MapProps.Center !^ (49.85, 14.06)
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
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
