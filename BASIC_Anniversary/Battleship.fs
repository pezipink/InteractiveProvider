module Battleship
open InteractiveProvider.Interfaces
open System
open System.Text

type Square =
    | Empty
    | Hit
    | Miss
    | Target
    override this.ToString() =
        match this with Empty | Target -> "_" | Hit -> "#" | Miss -> "O"

let gridWith (x,y) value (grid:_ array array) =
    let newGrid = Array.copy grid
    newGrid.[x].[y] <- value
    newGrid

let collides (x,y) (grid:_ array array) =
    match grid.[x].[y] with
    | Target             -> true
    | Empty | Hit | Miss -> false

let makeShot (x,y) (grid:_ array array) =
    match grid.[x].[y] with
    | Target     -> grid |> gridWith (x,y) Hit
    | Empty      -> grid |> gridWith (x,y) Miss
    | Hit | Miss -> grid

type BattleshipState =
    { human: Square array array; cpu:Square array array }
    interface IInteractiveState with
        member this.DisplayText =
            let sb = StringBuilder()
            let append (s:string) = sb.Append s |> ignore
            append "<summary>"
            this.human
            |> Array.iter (fun s ->
                    append "<para>"
                    s |> Array.iter (fun i ->
                    sb.AppendFormat("|{0}", i) |> ignore)
                    append "|</para>")
            append "</summary>"
            sb.ToString()
        member this.DisplayOptions =
            let options =
                this.cpu
                |> Array.mapi (fun i -> Array.mapi (fun j s -> (i, j, s)))
                |> Array.collect id
                |> Array.filter (fun (_,_,i) -> i <> Hit && i <> Miss)
                |> Array.map (fun (x,y,_) -> sprintf "Ensign, target X%i Y%i !" x y, box (x,y))
                |> List.ofArray
            ("# Battleship Grid", box (-1,-1)) :: options

type Battleship() =
    let random = Random().Next
    let initialGrid size = [| for i in 1..10 -> Array.create size Empty |]
    let gridCopy grid = [| for i in grid -> [| for j in i -> j |] |]
    let rec generateShips shipList grid =
        let rec addShip length =
            let horizontal = random 2 = 0
            let constant = random length
            let vary =  10 - length
            let dots = [|vary .. vary + length - 1|]
            let points =
                match horizontal with
                | true  -> Array.map (fun y -> constant,y) dots
                | false -> Array.map (fun x -> x,constant) dots
            match points |> Array.forall (fun i -> not <| collides i grid) with
            | true  -> Array.fold (fun g s -> gridWith s Target g) grid points
            | false -> addShip length
        match shipList with
        | ship :: rest -> generateShips rest <| addShip ship
        | []           -> grid
        
    interface IInteractiveServer with
        member this.NewState =
            { human=generateShips [2;5] <| initialGrid 10;
              cpu=generateShips [2;5] <| initialGrid 10 } :> IInteractiveState
        member this.ProcessResponse(state,choice) =
            let state = state :?> BattleshipState
            let human = gridCopy state.human
            let cpu = gridCopy state.human
            match unbox<int*int> choice with
            | (-1,-1) -> state :> IInteractiveState          
            | choice ->
                { state with human = human |> makeShot (random 10, random 10)
                             cpu   = cpu   |> makeShot choice } :> IInteractiveState