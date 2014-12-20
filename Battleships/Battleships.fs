// THIS IS UNFINISHED AND NOT REALLY PLAYABLE!!
module Battleship
open InteractiveProvider.Interfaces
open System
open System.Text
open System.Net.Sockets
open TCP

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

type PlayerType = Server of Socket | Client of Socket

type Square =
    | Water
    | Hit
    | Miss
    | Placing
    | Target
    override this.ToString() =
        match this with Water -> "_" | Placing -> "P" | Target -> "B" | Hit -> "#" | Miss -> "O"

type BattleShipGrid =  Square[,]

type CPUState = 
    { grid: BattleShipGrid 
      // TODO: put some stuff in here to make AI a bit more clever than not clever at all
    }
    with static member Blank = { grid = (Array2D.init 10 10 (fun _ _ -> Water )) }

type OpponentType = CPU of CPUState | Human of IPAddress

type Rotation = Horizontal | Vertical

let gridWith (x,y) value (grid:_ array array) =
    let newGrid = Array.copy grid
    newGrid.[x].[y] <- value
    newGrid

type TCPData = { mutable data : MessagePair<bool,bool> option}
    with static member Blank = { data = None }

type BattleShipStates =
    // initial spiel about battleships
    | Introduction
    // if CPU is picked here then the next stage is skipped
    | ChooseOpponent
    // Server or client?
    | ChoosePlayer
    // looped state that allows the player to enter an IP address via properties.
    | ExtractIPAddress of string * PlayerType
    //
    | InitialConnection of OpponentType * PlayerType * TCPData
    // looped state that allows players to move, rotate and place
    // each of their 5 BattleShips
    | SetupBattleships of ShipNumber * Location * Rotation * BattleShipGrid * OpponentType * PlayerType
    // attempt initial connection between players if applicable
    | Initialize of OpponentType * PlayerType  
    // the next two states cycle until game over,
    // YourTurn allows you to pick a location to shoot at 
    | YourTurn of BattleShipGrid * BattleShipGrid * OpponentType
    // fire actually does the firing and receives a response from the other TCP 
    // this is required to prevent the TP infrastructure attempting to fire
    // at all the available spots whilst it is generating the properties.
    | Fire of Location * BattleShipGrid * BattleShipGrid * OpponentType        
    // TheirTurn is a "waiting for other player" state which fires an invalidate
    // to the type provider on a response, and replies to the other TP indicating if
    // they hit anything    
    | TheirTurn of BattleShipGrid * BattleShipGrid * OpponentType    

    // Ronseal
    | GameOver of bool
    interface IInteractiveState with        
        member this.DisplayText =
            let sb = StringBuilder()
            let (~~) (s:string)  = sb.Append s     |> ignore                
            let (~~~) (s:string) = sb.AppendLine s |> ignore
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
            | ExtractIPAddress(current,player) -> 
                sprintf
                    "Enter the other player's IP address using the property interface.
                      
                     Current input : %s" current
            | InitialConnection(_,_,connected) ->
                match connected.data with
                | Some({result=true}) -> "Successfully connected!"
                | _ -> "Awaiting connection..."
            | SetupBattleships(ship, (y,x), rotation, grid, _, _) ->
                // show the current grid of placed ships, and the ship currently being placed 
                let shipSize = match ship with  2 -> 3 | 1 -> 2 | n -> n
                let tempGrid = Array2D.copy grid 

                // write the new ship over the grid (this is temporary)
                match rotation with
                | Horizontal -> for x = x to x + shipSize do tempGrid.[y,x] <- Placing
                | Vertical ->   for y = y to y + shipSize do tempGrid.[y,x] <- Placing

                for y in 0..9 do
                    for x in 0..9 do
                        ~~ "|"; ~~ tempGrid.[y,x].ToString()
                    ~~~ "|"

                let instructionText =
                    match ship with
                    | 5 -> "Place your aircraft carrier using the property interface"
                    | 4 -> "Place your battleship using the property interface"
                    | 3 -> "Place your submarine using the property interface"
                    | 2 -> "Place your destroyer using the property interface"
                    | 1 -> "Place your patrol boat using the property interface"
                    | _ -> failwith "!"
                ~~~ ""
                ~~~ instructionText
                Utils.wrapAndSplit (sb.ToString())
            | Initialize(ot,pt) ->
                // TODO: connection stuff here
                match ot with 
                | Human ipAdress -> ()
                | CPU grid -> ()
                ""
            | YourTurn(you,them,ot) ->
                ""
            | Fire(location, you, them, ot) ->
                ""
            | TheirTurn(you,them,ot) ->
                ""
            | GameOver winner -> 
                ""

        member this.DisplayOptions =
            match this with
            | Introduction -> 
                ["Continue", box 1]
            | ChooseOpponent ->                 
                ["CPU", box(CPU(CPUState.Blank)) ; "Human", box (Human "")]
            | ChoosePlayer -> 
                ["Server", box "Server"; "Client", box "Client"]
            | ExtractIPAddress(_,_) ->
                [for i in 0..9 -> sprintf "%i" i, box (i.ToString())
                 yield ".", box "."
                 yield "[Finish]", box "Finish"
                 yield "[Delete]", box "Delete"]
            | InitialConnection(_,_,connected) ->
                match connected.data with
                | Some({result=true}) -> ["Setup Battleships!", box "Setup"]
                | _ -> ["Keep waiting...", box "Wait"]
            | SetupBattleships(nextShip, (y,x), rotation, grid, _, _) -> 
                // note there are 2 ships sized 3 
                // only show the possible moves given the size of the ship
                let shipSize = match nextShip with
                               | 2 -> 3
                               | 1 -> 2
                               | n -> n
                
                let collides = 
                    let found = ref false
                    if rotation = Horizontal then 
                         for x = x to x + shipSize do if grid.[y,x] = Target then found := true
                    else for y = y to y + shipSize do if grid.[y,x] = Target then found := true
                    !found

                if rotation = Horizontal then
                    [ if x + shipSize < 9  then yield "Right", box "Right"
                      if y + 1 < 9 then yield "Down", box "Down"
                      if y > 0 then yield "Up", box "Up"
                      if x > 0 then yield "Left", box "Left"
                      if y + shipSize <9 then yield "Rotate", box "Rotate"
                      if not collides then yield "#Place", box "Place"]
                else
                    [ if y + shipSize < 9  then yield "Down", box "Down"
                      if y > 0 then yield "Up", box "Up"
                      if x > 0 then yield "Left", box "Left"
                      if x + 1 < 9 then yield "Right", box "Right"
                      if x + shipSize < 9 then yield "Rotate", box "Rotate"
                      if not collides then yield "#Place", box "Place"]
            | Initialize(ot,pt) ->
                // TODO.. waiting for a connection here, keep creating new Waiting.. properties
                match ot with
                | Human ipAddress -> ["Waiting for other player...", box "Waiting"]
                | CPU _ -> ["Commence Battle!", box "Continue"]
            | YourTurn(you,them,ip) ->
                // find all squares that have not yet been shot at 
                [yield "# Your Turn", box (-1,-1)
                 for y = 0 to 9 do 
                    for x = 0 to 9 do
                        if them.[y,x] = Water then 
                            yield sprintf "[%i,%i]" x y, box (y,x)]                
            | Fire(location,you,them,ip) -> 
                ["FIRE!", obj() ]
            | TheirTurn(you,them,ip) ->
                // if this is the cpu then make a random shot, otherwise wait for the other player to fire
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

type Battleships() =
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
            | Introduction -> ChooseOpponent 
            | ChooseOpponent -> 
                match (choice :?> OpponentType) with
                | CPU _ as CPU -> SetupBattleships(5, (0,0), Horizontal, Array2D.init 10 10 (fun _ _ -> Water), CPU , Server null)
                | Human _ -> ChoosePlayer 
            | ChoosePlayer -> 
                match (choice :?> string) with
                | "Server" -> ExtractIPAddress("",Server null) 
                | "Client" -> ExtractIPAddress("",Client null) 
                | _ -> failwith ""
            | ExtractIPAddress(current, player) -> 
                match (choice :?> string) with
                | "Finish" -> 
                    // TODO: think should try and wait for initial TCP connection here before proceeding
                    // maybe need another "Connecting..." state or similar?
                    InitialConnection(Human current, player,TCPData.Blank)
                | "Delete" -> ExtractIPAddress( (if current.Length = 0 then "" else current.Substring(0,current.Length-1)), player)
                | s -> ExtractIPAddress(current + s, player)
            | InitialConnection(ot,pt,mp) ->
                match (choice :?> string) with
                | "Setup" -> SetupBattleships(5,(0,0), Horizontal,Array2D.init 10 10 (fun _ _ -> Water), ot, pt)
                | "Wait" -> 
                    match pt, mp.data with
                    | Server _, None -> 
                        let (Human ip) = ot
                        let x = System.Threading.Thread.CurrentThread.ManagedThreadId                        
                        
                        if listening = false then
                            listening <- true
                            let socket = createSocket (SocketType.Server "192.168.0.2")
                            socket.Listen(10)
                            socket.Accept()
                            
                            //john.Post(AcceptClient (fun _ -> mp.data<-Some({message=false; result=false})))
                            
//                            async{
//                                let! handler = socket.AcceptAsync()
//                                mp.data<-Some({message=true; result=true})    
//                            } |> Async.Start
                            InitialConnection(ot,Server(socket),mp)
                        else
                            InitialConnection(ot,Server(socket),mp)
                    | Client _, None -> 
                        let (Human ip) = ot
                        // this will block here until connected... not much to do about it
                        let socket = createSocket (SocketType.Client ip) 
                        mp.data<-Some({message=true; result=true})    
                        InitialConnection(ot,Client(socket),mp)
                    | Server socket, Some({result=false})
                    | Client socket, Some({result=false}) ->
                        // keep waiting
                        printfn "waiting...."
                        InitialConnection(ot,pt,mp)
                    | _ -> failwith "" // this should be impossible
                | _ -> failwith ""
                    
                
            | SetupBattleships(ship, (y,x), rotation, grid, pt, ot) -> 
                match (choice :?> string) with
                | "Left"   -> SetupBattleships(ship,(y,x-1), rotation, grid, pt, ot)
                | "Right"  -> SetupBattleships(ship,(y,x+1), rotation, grid, pt, ot)
                | "Up"     -> SetupBattleships(ship,(y-1,x), rotation, grid, pt, ot)
                | "Down"   -> SetupBattleships(ship,(y+1,x), rotation, grid, pt, ot)
                | "Rotate" -> SetupBattleships(ship,(y,x), (if rotation = Horizontal then Vertical else Horizontal), grid, pt, ot)
                | "Place"  -> 
                    let newGrid = Array2D.copy grid
                    let shipSize = match ship with 2 -> 3 | 1 -> 2 | n -> n
                    if rotation = Horizontal then for x = x to x + shipSize do newGrid.[y,x] <- Target
                    else for y = y to y + shipSize do newGrid.[y,x] <- Target
                    if ship = 1 then 
                        // all ships placed, move to initialization / connection state
                        Initialize(pt,ot)
                    else SetupBattleships(ship-1,(0,0), Horizontal, newGrid, pt, ot)
                | _ -> failwith ""

            | Initialize(ot,pt) ->
                match ot with
                | Human ipAddress -> state
                | CPU _ -> state
            | YourTurn(you,them,ot) ->
                state
            | Fire(location,you,them,ot) -> 
                state
            | TheirTurn(you,them,ip) ->
                state
            | GameOver winner -> 
                state
            |> fun s -> s :> IInteractiveState




//
//
//
//let collides (x,y) (grid:_ array array) =
//    match grid.[x].[y] with
//    | Target             -> true
//    | Empty | Hit | Miss -> false
//
//let makeShot (x,y) (grid:_ array array) =
//    match grid.[x].[y] with
//    | Target     -> grid |> gridWith (x,y) Hit
//    | Empty      -> grid |> gridWith (x,y) Miss
//    | Hit | Miss -> grid
