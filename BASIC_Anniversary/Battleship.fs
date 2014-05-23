module Battleship
open InteractiveProvider.Interfaces
open System
open System.Text

type Square =
    | Empty
    | Hit
    | Target
    override this.ToString() =
        match this with Empty | Target -> " " | Hit -> "#"

let gridWith (x,y) value (grid:_ array array) =
    let newGrid = Array.copy grid
    newGrid.[x].[y] <- value
    newGrid

let shoot onHit onMiss (x,y) (grid:_ array array) =
    match grid.[x].[y] with
    | Target ->      onHit grid
    | Empty | Hit -> onMiss grid

let collides = shoot (fun _ -> true) (fun _ -> false)
let makeShot (x,y) = shoot (fun grid -> grid |> gridWith (x,y) Hit) id (x,y)

type BattleshipState =
    { human: Square array array; cpu:Square array array }
    interface IInteractiveState with
        member this.DisplayText =
            let sb = StringBuilder()
            let append (s:string) = sb.Append s |> ignore
            append "<summary>"
            this.human
            |> Array.iter (fun s ->
                s |> Array.iter (fun i ->
                    append "<para>"
                    sb.AppendFormat("|{0}", i) |> ignore
                    append "|</para>"))
            append "</summary>"
            sb.ToString()
        member this.DisplayOptions =
            let options =
                this.cpu
                |> Array.mapi (fun i -> Array.mapi (fun j s -> (i, j, s)))
                |> Array.collect id
                |> Array.filter (fun (_,_,i) -> i <> Hit)
                |> Array.map (fun (x,y,_) -> sprintf "Ensign, target X%i Y%i !" x y, box (x,y))
                |> List.ofArray
            ("# Battleship Grid", box (-1,-1)) :: options

type Battleship() =
    let random = Random().Next
    let intialGrid size = Array.create size <| Array.create size Empty
    let rec generateShips width height shipList grid =
        let rec addShip length =
            let horizontal = random 2 = 0
            let constant = random length
            let vary = length - height
            let dots = [vary .. vary + length - 1]
            let points =
                match horizontal with
                | true  -> Some [|1,2|]
                | false -> Some [||]
            match points with
            | Some points -> Array.fold (fun g s -> gridWith s Target g) grid points
            | None        -> addShip length
        match shipList with
        | ship :: rest -> generateShips width height rest <| addShip ship
        | []           -> grid
        
    interface IInteractiveServer with
        member this.NewState =
            { human=[||]; cpu=[||] } :> IInteractiveState
        member this.ProcessResponse(state,choice) =
            let state = state :?> BattleshipState
            match unbox<int*int> choice with
            | (-1,-1) -> state :> IInteractiveState          
            | choice ->
                { state with human = state.human |> makeShot (random 10, random 10)
                             cpu   = state.cpu   |> makeShot choice } :> IInteractiveState