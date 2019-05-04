#load "../.paket/load/netstandard2.0/main.group.fsx"

#if INTERACTIVE
// #r "netstandard"
#r @"C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\netstandard.dll"
#endif

open Fable.React
open Fable.React.Props
open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop

importSideEffects "./Style.css"

type ReduceFn<'state,'msg> = ('state -> 'msg -> 'state)  
type Dispatch<'msg> ='msg -> unit  
let useReducer<'state,'msg> (reducer: ReduceFn<'state,'msg>) (initialState:'state) : ('state * Dispatch<'msg>)  = import "useReducer" "react"

module Array =
    let swap<'t> (arr: 't array) a b =
        let tmp = arr.[a]
        arr.[a] <- arr.[b]
        arr.[b] <- tmp
        arr

let mimimumSwap (input: int array) =
    let annotated =
        input
        |> Array.indexed
        |> Array.sortBy snd
        
    let length =  Array.length input
        
    let mutable i = 0
    
    seq {
        while (i < length) do
            if (fst annotated.[i]) = i then
                i <- i + 1
            else
                yield (i, fst annotated.[i])
                Array.swap annotated i (fst annotated.[i]) |> ignore
    }
    |> Seq.toList
    |> List.rev

let toStr (a:int) = a.ToString()

type Move = int * int

// Elmish

type Msg =
    | FirstTileClicked of int
    | SecondTileClicked of int
    | Solve
    
type Model = 
    { Input: int array
      SelectedTile: int option
      Moves: Move list }

let init = 
    { Input = [|1..9|]
      SelectedTile = None
      Moves = [] }

let tap a = 
    printfn "%A" a
    a

[<Emit("$0.slice(0)")>]
let cloneArray<'t> (input: 't array) : 't array = jsNative    

let update model msg =
    printfn "%A" msg
    match msg with
    | FirstTileClicked idx -> { model with SelectedTile = Some idx; Moves = [] }
    | SecondTileClicked b -> 
        match model.SelectedTile with
        | None -> model
        | Some a ->
            let swapped = Array.swap model.Input a b
            { model with Input = swapped; SelectedTile = None; Moves = [] }
    | Solve ->
        let input = cloneArray model.Input
        let moves = mimimumSwap input
        { model with Moves = moves; SelectedTile = None }

// End Elmish

type InputTileProps =
    { Key: int
      OnClick: MouseEvent -> unit }

let InputTile = 
    FunctionComponent.Of (fun (props:InputTileProps) ->
       img [sprintf "./tiles/%d.jpg" (props.Key + 1) |> Src; OnClick props.OnClick]
    , "Tile")

type InputProps = { Dispatch: Msg -> unit; Model: Model }

let Input = 
    FunctionComponent.Of (fun ({Model = model; Dispatch = dispatch}: InputProps) -> 
        let handleClick idx ev =
            match model.SelectedTile with
            | None -> dispatch (FirstTileClicked idx)
            | Some _ -> dispatch (SecondTileClicked idx)

        let rows = 
            Array.chunkBySize 3 model.Input
            |> Array.mapi (fun idx rows ->
                let tiles =
                    [|0..2|]
                    |> Array.map (fun r ->
                        let tIdx = idx * 3 + r
                        let tV = rows.[r] - 1
                        InputTile { Key = tV; OnClick = handleClick tIdx }
                    )

                div [Key (toStr idx) ] tiles
            )

        div [] rows
    , "Input")

let moveTileClassName current source target =
    if current = source then
        "source"
    else if current = target then
        "target"
    else
        "invariant"

type MoveProps = 
    { Index: int
      Current: int array
      Source: int
      Key : string
      Target: int }

let Move  =
    FunctionComponent.Of (fun ({ Index = idx; Current = current; Source = s; Target = t}) -> 
        let rows =
            Array.chunkBySize 3 current
            |> Array.mapi (fun rIdx rows ->
                let tiles = 
                    rows
                    |> Array.map (fun tile ->
                            let key = sprintf "move-%d-row-%d-%d" idx rIdx tile
                            let url = sprintf "url(./tiles/%d.jpg)" tile
                            let currentIndex =  Array.findIndex ((=) tile) current
                            let className = moveTileClassName currentIndex s t
                            div [ Key key; ClassName className; Style [CSSProp.BackgroundImage url]] []
                    )

                div [ClassName "move-row"] tiles
            )

        div [ClassName "move"] [
            yield h5 [] [sprintf "Step %d" (idx+1) |> str ]
            yield! rows
        ]
    , "Move")


let app _ =
    let (model,dispatch) = useReducer update init

    let executedMoves =
        let input = cloneArray model.Input

        model.Moves
        |> List.scan (fun acc (s,t) -> 
            let input = cloneArray acc
            Array.swap input s t
        ) input

    let results =
        List.zip model.Moves executedMoves
        |> List.mapi (fun idx ((s,t), result) ->
            Move { Index = idx; Source = s; Target = t; Current = result; Key =(sprintf "move-%d" idx) }
        )

    div [ClassName "container-fluid"] [
        h1 [] [str "Ronny Solver"]
        div [ ClassName "row" ] [
            div [ ClassName "col-sm-4" ] [
                h2 [] [str "Input"]
                Input { Dispatch = dispatch; Model = model }
                button [ClassName "btn btn-dark btn-block"; OnClick (fun _ -> dispatch Solve)] [str "Solve"]
            ]
            div [ ClassName "col-sm-8" ] results
        ]
        div [ ClassName "row"] [
            div [ ClassName "col-sm-12 mt-4 bg-warning p-2 d-none" ] [
                code [ ] [
                    pre [] [
                        Fable.Core.JS.JSON.stringify(model, space = Some 4)
                        |> str
                    ]
                ]
            ]
        ]
    ]

ReactDom.render(FunctionComponent.Of (app , "App") (), document.getElementById("app"))