module Battleship
open InteractiveProvider.Interfaces
open System
open System.Text

module Utils =
    let rnd =  System.Random(System.DateTime.Now.Millisecond)
    let wrapAndSplit (text:string) =
        let text = 
            text.Split([|Environment.NewLine|], StringSplitOptions.None)
            |> Array.map(fun s -> String.Format("<para>{0}</para>", s.Replace(" "," ")))
            |> fun t ->  String.Join("\r\n", t)
        String.Format("<summary>{0}</summary>", text)

// this type provider is a bit fun to understand as you 
// have to have in your head at all times that this will be running 
// on both machines in different modes (server / client)


//type GameType =
//    | Normal
//    | Salvo
//    | Volley

type IPAddress = string

type Location = int * int

type ShipNumber = int

type Direction = Left | Up | Down | Right

type PlayerType = Server | Client

type OpponentType = CPU | Human of IPAddress

type Rotation = Horizontal | Vertical

type Square =
    | Empty
    | Hit
    | Miss
    | Placing
    | Target
    override this.ToString() =
        match this with Empty -> "_" | Placing -> "P" | Target -> "B" | Hit -> "#" | Miss -> "O"

type BattleShipGrid =  Square[,]


let gridWith (x,y) value (grid:_ array array) =
    let newGrid = Array.copy grid
    newGrid.[x].[y] <- value
    newGrid


type BattleShipStates =
    // initial spiel about battleships
    | Introduction
    // if CPU is picked here then the next stage is skipped
    | ChooseOpponent
    // Server or client?
    | ChoosePlayer
    // looped state that allows the player to enter an IP address via properties.
    | ExtractIPAddress 
    // looped state that allows players to move, rotate and place
    // each of their 5 BattleShips
    | SetupBattleships of ShipNumber * Location * Rotation * BattleShipGrid * OpponentType
    // attempt initial connection between players if applicable
    | Initialize of PlayerType * OpponentType    
    // the next two states cycle until game over,
    // YourTurn allows you to pick a location to shoot at and receive
    // a response indicating if you hit anything
    | YourTurn of BattleShipGrid * BattleShipGrid * IPAddress
    // TheirTurn is a "waiting for other player" state which fires an invalidate
    // to the type provider on a response, and replies to the other TP indicating if
    // they hit anything    
    | TheirTurn of BattleShipGrid * BattleShipGrid * IPAddress
    // Ronseal
    | GameOver of bool
    interface IInteractiveState with
        member this.DisplayText =
            match this with
            | Introduction -> 
                "Welcome to Type Provider battleships!
                In this classic game you are able to play against the CPU,
                or against a friend via the TCP/IP protocol.
                
                Simply decide who will be the server and client, enter each other's 
                IP addresses and commence war via your choice of IDE!
                "
            | ChooseOpponent ->                 
                "Choose your opponent"
            | ChoosePlayer -> 
                "Are you the server or the client?"
            | ExtractIPAddress -> 
                "Enter the other player's IP address using the property interface"
            | SetupBattleships(nextShip, (x,y), rotation, grid, _) ->
                // show the current grid of placed ships, and the ship currently being placed 
                let sb = StringBuilder()
                let (~~) (s:string)  = sb.Append s     |> ignore                
                let (~~~) (s:string) = sb.AppendLine s |> ignore
                
                let shipSize = match nextShip with
                               | 2 -> 3
                               | 1 -> 2
                               | n -> n
               
                let tempGrid = Array2D.copy grid 

                // write the new ship over the grid (this is temporary)
                match rotation with
                | Horizontal -> 
                    for x = x to shipSize do tempGrid.[x,y] <- Placing
                | Vertical -> 
                    for x = x to shipSize do tempGrid.[x,y] <- Placing

                for x in 0..9 do
                    for y in 0..9 do
                        ~~ "|"; ~~ tempGrid.[x,y].ToString()
                    ~~~ "|"

                let instructionText =
                    match nextShip with
                    | 5 -> "Place your aircraft carrier using the property interface"
                    | 4 -> "Place your battleship using the property interface"
                    | 3 -> "Place your submarine using the property interface"
                    | 2 -> "Place your destroyer using the property interface"
                    | 1 -> "Place your patrol boat using the property interface"
                    | _ -> failwith "!"
                ~~~ ""
                ~~~ instructionText
                Utils.wrapAndSplit (sb.ToString())
            | Initialize(pt,ot) ->
                // TODO: connection stuff here
                ""
            | YourTurn(you,them,ip) ->
                ""
            | TheirTurn(you,them,ip) ->
                ""
            | GameOver winner -> 
                ""

//            append "<summary>"
//            this.human
//            |> Array.iter (fun s ->
//                    append "<para>"
//                    s |> Array.iter (fun i ->
//                    sb.AppendFormat("|{0}", i) |> ignore)
//                    append "|</para>")
//            append "</summary>"
//            sb.ToString()
        member this.DisplayOptions =
            match this with
            | Introduction -> 
                ["Continue", box 1]
            | ChooseOpponent ->                 
                ["CPU", box CPU; "Human", box Human]
            | ChoosePlayer -> 
                ["Server", box Server; "Client", box Client]
            | SetupBattleships(nextShip, location, rotation, grid, _) -> 
                // note there are 2 ships sized 3 


                []
            | Initialize(pt,ot) ->
                []
            | YourTurn(you,them,ip) ->
                []
            | TheirTurn(you,them,ip) ->
                []
            | GameOver winner -> 
                []

//            let options =
//                this.cpu
//                |> Array.mapi (fun i -> Array.mapi (fun j s -> (i, j, s)))
//                |> Array.collect id
//                |> Array.filter (fun (_,_,i) -> i <> Hit && i <> Miss)
//                |> Array.map (fun (x,y,_) -> sprintf "Ensign, target X%i Y%i !" x y, box (x,y))
//                |> List.ofArray
//            ("# Battleship Grid", box (-1,-1)) :: options

type Battleship() =
//    let random = Random().Next
//    let initialGrid size = [| for i in 1..10 -> Array.create size Empty |]
//    let gridCopy grid = [| for i in grid -> [| for j in i -> j |] |]
//    let rec generateShips shipList grid =
//        let rec addShip length =
//            let horizontal = random 2 = 0
//            let constant = random length
//            let vary =  10 - length
//            let dots = [|vary .. vary + length - 1|]
//            let points =
//                match horizontal with
//                | true  -> Array.map (fun y -> constant,y) dots
//                | false -> Array.map (fun x -> x,constant) dots
//            match points |> Array.forall (fun i -> not <| collides i grid) with
//            | true  -> Array.fold (fun g s -> gridWith s Target g) grid points
//            | false -> addShip length
//        match shipList with
//        | ship :: rest -> generateShips rest <| addShip ship
//        | []           -> grid
        
    interface IInteractiveServer with
        member this.NewState = Introduction :> IInteractiveState
        member this.ProcessResponse(state,choice) =
            let state = state :?> BattleShipStates
            
            match state with
            | Introduction -> state
            | ChooseOpponent -> state
            | ChoosePlayer -> state
            | SetupBattleships(nextShip, location, rotation, grid, _) -> 
                state
            | Initialize(pt,ot) ->
                state
            | YourTurn(you,them,ip) ->
                state
            | TheirTurn(you,them,ip) ->
                state
            | GameOver winner -> 
                state
            |> fun s -> s :> IInteractiveState







let collides (x,y) (grid:_ array array) =
    match grid.[x].[y] with
    | Target             -> true
    | Empty | Hit | Miss -> false

let makeShot (x,y) (grid:_ array array) =
    match grid.[x].[y] with
    | Target     -> grid |> gridWith (x,y) Hit
    | Empty      -> grid |> gridWith (x,y) Miss
    | Hit | Miss -> grid
