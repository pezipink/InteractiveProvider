namespace Xmas

open InteractiveProvider.Interfaces
open System

module Utils =
    let rnd = Random(DateTime.Now.Millisecond)
    let D x y = rnd.Next(x, x * y + 1)
    let wrapAndSplit (text:string) =
        let text = 
            let blank = " "
            text.Split([|Environment.NewLine;"\n"|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map(fun s -> String.Format("<para>{0}</para><para>{1}</para>", s.Replace(" "," ").Replace("-","~"),blank))
            |> fun t ->  String.Join("\r\n", t)
        String.Format("<summary>{0}</summary>", text)

open Utils

type Direction =
    | North 
    | NorthEast
    | East 
    | SouthEast
    | South
    | SouthWest
    | West 
    | NorthWest 
    with static member AllDirctions = [North;NorthEast;East;SouthEast;South;SouthWest;West;NorthWest]

type Tile =
    | Empty
    | RoomFloor
    | Corridor
    | HWall
    | VWall
    | Door of bool 
    | Stairs of bool // up?
    with 
    member this.Draw =
        match this with
        | Empty -> " "
        | RoomFloor -> "."
        | Corridor -> "#"
        | HWall -> "="
        | VWall -> "|"
        | Door true -> "+"
        | Door false -> "/"
        | Stairs true -> "&gt;"
        | Stairs false -> "&lt;"

type Point = 
    { X:int; 
      Y:int }
    with
        member this.DistanceTo(other:Point) =
            let x = (other.X - this.X) 
            let y = (other.Y - this.Y) 
            int(sqrt(float(x*x + y*y)))
        member this.ManhattanDistanceTo(other:Point) =
            abs(this.X-other.X) + abs(this.Y-other.Y)

type PieType =
    | Healing
    | Poison

type CarrotValue =
    | Amazing
    | Rotten
    | NotSoGood
    
type ItemType =
    | MincedPie of PieType
    | Carrot of CarrotValue 
    | Presents of int            
    with 
        member this.Draw =
            match this with
            | MincedPie _ -> "!"
            | Carrot _ -> "%"
            | Presents _ -> "*"        

        member this.Text =
            match this with
            | MincedPie _ -> "an unidentified minced pie"
            | Carrot Rotten -> "a rotten carrot here"
            | Carrot NotSoGood -> "a carrot that looks past its best"
            | Carrot Amazing -> "an awesome looking carrot"
            | Presents i when i = 1 ->"a present"
            | Presents i -> sprintf "a pile of %i presents" i

type Item =
    { itemType:ItemType
      loc:Point option}
                  
type EntityType = 
    | Santa
    | Reindeer
    | Elf
    with
    member this.Draw =
        match this with
        | Santa -> "@"
        | Elf -> "E"
        | Reindeer -> "R"

type Entity =
    { loc:Point; etype : EntityType; active:bool; health:int*int }

module DungeonGenerator =
    type Rect = {Location:Point; Width:int; Height:int }
    // this is a fairly terrible dungeon generator that 
    // randomly attempts to create rooms and then corridors between them    
    let private intersects rectA rectB =
       not (rectA.Location.X > rectB.Location.X + rectB.Width + 1  ||
            rectA.Location.X + rectA.Width < rectB.Location.X + 1  ||
            rectA.Location.Y > rectB.Location.Y + rectB.Height + 1 ||
            rectA.Location.Y + rectA.Height < rectB.Location.Y + 1 ) 

    let generateDungeon width height =
        let rectCentre rect =
            { X=rect.Location.X+(rect.Width/2); Y=rect.Location.Y+(rect.Height/2)}

        // first create a bunch of rooms that do not overlap with each other
        let rec createRooms acc numAttemps = 
            let createRoom() =
                let w = rnd.Next(4,15)
                let h = rnd.Next(4,15)
                { Location={X=rnd.Next(11,width-1-w);Y=rnd.Next(1,height-h)};Width=w;Height=h} 
        
            if numAttemps = 0 then acc
            else
                let room = createRoom()
                if acc |> List.exists(fun other -> 
                    intersects { room with Location={X=room.Location.X-2;Y=room.Location.Y-2}; Width=room.Width+5; Height=room.Height+5 } other ) 
                then 
                    createRooms acc (numAttemps-1) 
                else createRooms (room::acc) (numAttemps-1)
    
        // create intial set of rooms 
        let roomRects = createRooms [] 50

        // work out a set of points that represent the walls 
        let walls = 
            roomRects 
            |> List.fold( fun acc r -> 
                [for x = r.Location.X to r.Location.X + r.Width do
                    for y = r.Location.Y to r.Location.Y + r.Height do
                        if y = r.Location.Y || y = r.Location.Y + r.Height then yield ({X=x;Y=y},true) 
                        elif x = r.Location.X || x = r.Location.X + r.Width then yield ({X=x;Y=y},false)] @ acc ) []

        let randomWalk source dest =
            // move towards the destination one step at a time randomly converging on x or y 
            Seq.unfold 
                (fun state -> 
                    let hwall = walls |> List.exists(fun (p,h) -> p=state && h) 
                    let vwall = walls |> List.exists(fun (p,h) -> p=state && h = false) 
                    let xv = if state.X - dest.X < 0 then 1 elif state.X - dest.X > 0 then -1 else 0
                    let yv = if state.Y - dest.Y < 0 then 1 elif state.Y - dest.Y > 0 then -1 else 0
                    let r = if rnd.Next(2) = 1 then -1 else 1
                    match xv,yv with 
                    | 0,0 -> None
                    | 0,y -> let n = 
                                if vwall then {state with X=state.X+r; Y=state.Y }
                                else {state with X=state.X; Y=state.Y+y }
                             Some(n,n)
                    | x,0 -> let n = 
                                if hwall then {state with X=state.X; Y=state.Y+r }
                                else { state with X=state.X+x; Y=state.Y }
                             Some(n,n)
                    | x,y -> let n = 
                                if vwall then {state with X=state.X+x; Y=state.Y }
                                elif hwall then {state with X=state.X; Y=state.Y+y }
                                elif rnd.Next(2) = 1 then  { state with X=state.X; Y=state.Y+y } 
                                else { state with X=state.X+x; Y=state.Y }
                             Some(n,n)
                    ) source      
            |> Seq.toList

        //create corridors between rooms 
        let corridors = 
            roomRects 
            |> Seq.pairwise
            |> Seq.fold( fun acc (a,b) -> 
                let c1 = rectCentre a
                let c2 = rectCentre b
                let corridor = 
                    randomWalk c1 c2
                    |> List.map( fun p -> (p, Tile.Corridor)  )
                acc @ corridor 
            ) []
    
        let rooms =
            roomRects |> List.fold( fun acc r -> 
                let temp = 
                    [for x = r.Location.X to r.Location.X + r.Width do
                        for y = r.Location.Y to r.Location.Y + r.Height do
                            let p  = {X=x;Y=y}
                            if y = r.Location.Y || y = r.Location.Y + r.Height then 
                                yield (p, Tile.HWall)
                            elif x = r.Location.X || x = r.Location.X + r.Width then 
                                yield (p, Tile.VWall)
                            else yield (p, Tile.RoomFloor)]
                temp :: acc
                ) []
        let allRooms = List.collect id rooms
        // transform any corridor locactions that are wall locations into doors
        let corridors = 
            ((Set.empty,[]),corridors)
            ||> List.fold( fun acc (cp,e) -> 
                let (doors,acc) = acc
                if  walls |> List.exists( fun (p,_) -> p=cp) then
                    // only create a door if there isn't one already next to us
                    let points = Set [for x = cp.X - 1 to cp.X + 1 do
                                        for y = cp.Y - 1 to cp.Y + 1 do
                                            yield {X=x;Y=y}] 
                    if (Set.intersect doors points).Count = 0 then 
                        (Set.add cp doors, (cp,Tile.Door false) :: acc)
                    else 
                        (doors,(cp,e) :: acc) 
                else 
                    (doors,(cp,e) :: acc) ) 
            |> snd

        //finally place the up and down stairs and the player
        let stairsUp =
            let room = roomRects.Head 
            let position = {X=rnd.Next(room.Location.X + 1, room.Location.X + room.Width);Y=rnd.Next(room.Location.Y + 1, room.Location.Y + room.Height)}
            (position ,Tile.Stairs true) 

        let stairsDown =
            let room = List.rev roomRects |> List.head
            let position = {X=rnd.Next(room.Location.X + 1, room.Location.X + room.Width);Y=rnd.Next(room.Location.Y + 1, room.Location.Y + room.Height)}
            (position ,Tile.Stairs false) 

        //remove any corridor locations that are also room locations which are not doors
        let corridors = corridors |> List.filter( fun (p,v) -> 
            not(allRooms 
                |> List.exists( fun (p',v') -> v <> (Door true) &&  v <> (Door false)
                                               && p = p')))

        let allRooms = allRooms |> List.filter( fun (p,v) -> 
            not(corridors 
                |> List.exists( fun (p',v') ->  p = p' )) && 
                                                p <> (fst stairsUp) && 
                                                p <> (fst stairsDown) )


        // place some monsters around the level
        let monsters = 
            [for i in 1..(D 5 2) do
                // pick a random room ..
                let room = roomRects.[rnd.Next(roomRects.Length)]
                // pick a spot in the room which isnt a wall
                let x = rnd.Next(room.Location.X + 1, room.Location.X + room.Width)
                let y = rnd.Next(room.Location.Y + 1, room.Location.Y + room.Height)
                let p = {X=x;Y=y}
                if p <> (fst stairsUp) then yield p]
            |> Set.ofList
            |> Set.toList
            |> List.map(fun p -> 
                    if (D 1 2) = 1 then{loc=p;etype=Reindeer;active=false;health=(16,16)} 
                    else {loc=p;etype=Elf;active=false;health=(16,16)} )

        // place some items around the level
        let getRandomItem() =
            let r = D 1 3
            if r = 1 then 
                let t = D 1 2
                if t = 1 then MincedPie Healing
                else MincedPie Poison
            elif r = 2 then
                let t = D 1 3
                if t = 1 then Carrot Amazing
                elif t = 2 then Carrot Rotten
                else Carrot NotSoGood
            else
                Presents (D 1 20)    

        let items = 
            [for i in 1..(D 5 2) do
                // pick a random room ..
                let room = roomRects.[rnd.Next(roomRects.Length)]
                // pick a spot in the room which isnt a wall
                let x = rnd.Next(room.Location.X + 1, room.Location.X + room.Width)
                let y = rnd.Next(room.Location.Y + 1, room.Location.Y + room.Height)
                let p = {X=x;Y=y}
                if p <> (fst stairsUp) then yield p]
            |> Set.ofList
            |> Set.toList
            |> List.map(fun p -> {loc=Some p;itemType = getRandomItem() } )


        (stairsDown :: stairsUp :: allRooms @ corridors), rooms, stairsDown, monsters, items

module GrottoCore =   
    let height = 35
    let width = 70
    let translatePosition position direction =
        let x = position.X
        let y = position.Y
        match direction with 
        | North     -> (x,y - 1)
        | NorthEast -> (x + 1,y - 1) 
        | East      -> (x + 1,y) 
        | SouthEast -> (x + 1,y + 1) 
        | South     -> (x,y + 1) 
        | SouthWest -> (x - 1,y + 1) 
        | West      -> (x - 1,y ) 
        | NorthWest -> (x - 1,y - 1) 
        |> fun (x,y ) -> {X=x;Y=y}

    let calculateFov (point:Point) (map:Map<Point,Tile>) (rooms:(Point*Tile) list list)=
        if map.ContainsKey point = false then Set.empty else
        let rec unfoldLine point direction count acc =
            // this functions walks in a straight line for 3 tiles unless stopped 
            // by walls or empty tiles
            if count > 3 then acc else
            let newPoint = translatePosition point direction                
            match map.TryFind newPoint with
            | None
            | Some Empty -> acc // stop here, can't see further
            | Some HWall
            | Some VWall -> point :: acc // stop here but include this tile
            | _ -> unfoldLine newPoint direction (count+1) (point :: acc)
        match map.[point] with
        | Corridor 
        | Door _ ->
            // for a corridor we can simply see 3 tiles n, s, e, w along it
            Direction.AllDirctions
            |> List.map(fun d -> unfoldLine point d 0 [] )
            |> List.collect id
            |> Set.ofList
        | RoomFloor 
        | Stairs _ ->
            // just show all the room if in a room 
            let room = rooms |> List.find (fun r -> List.exists(fun (p,_)-> p = point) r)
            room |> List.map fst |> Set.ofList
        | _ -> Set.empty


type Responses =
    | Start
    | Introduction
    | Nothing
    | Move of Direction
    | More
    | OpenDoor of Direction option
    | CloseDoor of Direction option
    | UseItem of Item option
    | Pickup

type GameState = 
    | NewState
    | MainState
    | GetDirectionState of Responses
    | GetBoolState of Responses
    | GetItemState of Responses

type Santa =
    { entity : Entity
      hunger : int
      inventory : Item list
      lastHealTurn : int
      exp : int
      presents:int}

type SantasGrottoState =
    { state:GameState; entities:Entity list; tiles:Map<Point,Tile>; status : string list; 
    discoveredTiles : Set<Point>; rooms:(Point*Tile)list list; currentFov:Set<Point>; 
    santa:Santa; turnsElapsed : int; items:Item list; grottoLevel : int
     }
      interface IInteractiveState with
            member this.DisplayText = 
                  let toText () = 
                      let eLookup = this.santa.entity :: this.entities |> List.map(fun e -> e.loc, e ) |> Map.ofList
                      let iLookup = this.items |> List.map(fun e -> e.loc, e ) |> Map.ofList
                      let sb = System.Text.StringBuilder()
                      sb.Append("<summary>") |> ignore
                      for y = 0 to GrottoCore.height do                      
                        sb.Append "<para>" |> ignore
                        for x = 0 to GrottoCore.width do
                            // display previously discovered tiles, and anything else that is currently visible.
                            // this is slow but who cares
                            let showTile p = if Set.contains p this.discoveredTiles then true else false
                            let point = {X=x;Y=y}
                            match eLookup.TryFind(point) with
                            | Some e when this.currentFov.Contains point -> sb.Append e.etype.Draw |> ignore
                            | _ ->
                                match iLookup.TryFind(Some point) with
                                | Some e when this.currentFov.Contains point -> sb.Append e.itemType.Draw |> ignore
                                | _ ->
                                    match this.tiles |> Map.tryFind point with
                                    | Some e when showTile point -> sb.Append e.Draw |> ignore
                                    | _ -> sb.Append " " |> ignore
                        sb.Append "</para>" |> ignore
                      let hungry = if this.santa.hunger < 10 then "you are starving to death!"
                                   elif this.santa.hunger < 30 then "you are hungry"
                                   else ""
                      sb.AppendFormat("<para>Health:{0}({1})Exp:{2} | {3} {4}</para>",(fst this.santa.entity.health),(snd this.santa.entity.health), this.santa.exp, this.santa.exp, hungry   ) |> ignore 
                      let status, t = match this.status with
                                      | h :: t -> h, t
                                      | _ -> " ",[]
                      let status2 = match t with
                                    | h :: _ -> h
                                    | _ -> " "
                      
                      sb.Append(sprintf "<para>%s</para>" status) |> ignore 
                      sb.Append(sprintf "<para>%s</para>" status2) |> ignore 
                      sb.Append("</summary>") |> ignore 
                      sb.ToString()  
                  
                  match this.state with
                  | NewState -> 
                        """Welcome to the Escape From Santa's Grotto type provider!
Created December 2014 by Ross McKinlay, pinksquirrellabs.com 
You can find the source at my github (pezipink), pull requests welcome!
Poor Saint Nick had one too many sherries and has woken up at the bottom floor
of his workshop and grotto!  Unfortunately, it seems that his elves, reindeer 
and a host of other creatures have also been on the sherry and are rampaging around.
Can you help jolly old Father Christmas to fight his way out and escape the grotto?

This roguelike game was developed in the spirit of the original Rogue, 1980~ ! 
"""
                  | _ -> toText() 
                  |> Utils.wrapAndSplit
            member this.DisplayOptions = 
                if this.status.Length > 2 then ["# Grotto View", Nothing :>_; "More", More :>_] else
                match this.state with
                | NewState -> ["# Introduction", Introduction :> _ ; "Enter the Grotto!",Start :> _ ]
                | MainState   -> 
                    ["# Grotto View", Nothing  :>_
                     "N",     (Move North    ) :>_
                     "NE",    (Move NorthEast) :>_
                     "E",     (Move East     ) :>_
                     "SE",    (Move SouthEast) :>_
                     "S",     (Move South    ) :>_
                     "SW",    (Move SouthWest) :>_
                     "W",     (Move West     ) :>_
                     "NW",    (Move NorthWest) :>_
                     "Open",  (OpenDoor None) :>_ 
                     "Close", (CloseDoor None) :>_ 
                     "Use",   (UseItem None) :>_ 
                     "Pickup",(Pickup) :>_ ]
                | GetBoolState response -> 
                    ["# Grotto View", Nothing  :>_
                     "Yes",  response :>_
                     "No",   response :>_ ]
                | GetItemState _ -> 
                    let items = this.items |> List.filter(fun i -> i.loc.IsNone && match i.itemType with Presents _ -> false | _ -> true)
                                           |> List.map(fun i -> i.itemType.Text, (UseItem(Some i) :> obj))

                    ("# Use which item?", Nothing  :>_) :: items
                | GetDirectionState response -> 
                    match response with                    
                    | OpenDoor _ -> 
                        ["# Open in which direction?", Nothing  :>_
                         "N",     (OpenDoor (Some North    )) :>_
                         "NE",    (OpenDoor (Some NorthEast)) :>_
                         "E",     (OpenDoor (Some East     )) :>_
                         "SE",    (OpenDoor (Some SouthEast)) :>_
                         "S",     (OpenDoor (Some South    )) :>_
                         "SW",    (OpenDoor (Some SouthWest)) :>_
                         "W",     (OpenDoor (Some West     )) :>_
                         "NW",    (OpenDoor (Some NorthWest)) :>_]
                    | CloseDoor _ -> 
                        ["# Close in which direction?", Nothing  :>_
                         "N",     (CloseDoor (Some North    )) :>_
                         "NE",    (CloseDoor (Some NorthEast)) :>_
                         "E",     (CloseDoor (Some East     )) :>_
                         "SE",    (CloseDoor (Some SouthEast)) :>_
                         "S",     (CloseDoor (Some South    )) :>_
                         "SW",    (CloseDoor (Some SouthWest)) :>_
                         "W",     (CloseDoor (Some West     )) :>_
                         "NW",    (CloseDoor (Some NorthWest)) :>_]
                    | _ -> ["ERROR", Nothing :>_]
                

type ``Santa's Grotto``() =
    let rnd = System.Random(System.DateTime.Now.Millisecond)
    
    interface IInteractiveServer with
        member this.NewState = 
            { state = NewState; entities = []; tiles = Map.empty; status = ["Welcome to Level 1, Saint Nick!"]; 
              discoveredTiles = Set.empty; rooms = []; currentFov = Set.empty 
              santa= Unchecked.defaultof<_>; turnsElapsed = 0; items=[]; grottoLevel=1 } :> IInteractiveState
        member this.ProcessResponse(client,choice) =
            let state = client :?> SantasGrottoState            

            let endTurn state = 
                // todo: run ai here
                let health = if state.santa.lastHealTurn + state.turnsElapsed > 10 && (fst state.santa.entity.health) < (snd state.santa.entity.health)   then 
                                (fst state.santa.entity.health) + 1, (snd state.santa.entity.health) 
                             else state.santa.entity.health

                let health = if state.santa.hunger <= 0 then (fst health) + -1, (snd health) 
                             else health

                let santa = { state.santa with hunger = state.santa.hunger - 1; entity = {state.santa.entity with health = health } }
                {state with turnsElapsed = state.turnsElapsed + 1 }

            match state.state with
            | NewState -> 
                match unbox<Responses> choice with
                | Introduction -> state :> IInteractiveState          
                | Start ->                     
                    let dungeon, rooms, startLocation, monsters, items = DungeonGenerator.generateDungeon GrottoCore.width GrottoCore.height 
                    let dungeon = dungeon |> Map.ofList
                    let fov = GrottoCore.calculateFov (fst startLocation) dungeon rooms
                    let santa = { entity={loc=(fst startLocation);etype=Santa;active=true;health=(16,16)} ; hunger = 50; inventory = []; lastHealTurn = 0; exp = 0; presents=0 } 
                    { state with state = MainState; tiles=dungeon;rooms=rooms;currentFov=fov; discoveredTiles=fov;entities=monsters;santa=santa;items=items} :>_
                | _ -> state :>_
            | GetItemState _ -> 
                match unbox<Responses> choice with
                | UseItem(Some item) -> 
                    let santa, status =
                        match item.itemType with
                        | MincedPie Healing -> 
                           let amt = D 1 6
                           let amt = if fst state.santa.entity.health  + amt > snd state.santa.entity.health then snd state.santa.entity.health  - fst state.santa.entity.health 
                                     else amt
                           { state.santa with entity = { state.santa.entity with health = amt, (snd state.santa.entity.health ) }}, "you feel somewhat better!"
                        | MincedPie Poison -> 
                           let amt = D 1 6                           
                           { state.santa with entity = { state.santa.entity with health = (fst state.santa.entity.health - amt), (snd state.santa.entity.health ) } }, "yuck! that was a bad mince pie...."
                        | Carrot Amazing -> 
                           let amt = D 10 2
                           {state.santa with hunger = state.santa.hunger + amt}, "you feel less hungry"
                        | Carrot NotSoGood -> 
                           let amt = D 5 2
                           {state.santa with hunger = state.santa.hunger + amt}, "you feel somewhat less hungry"
                        | Carrot Rotten -> 
                           let amt = D 2 2
                           {state.santa with hunger = state.santa.hunger + amt}, "you feel a little less hungry"
                        | _ -> state.santa, "you cannot use that" // this should not happen
                    let items = state.items |> List.filter((<>)item)
                    { state  with state = MainState; santa=santa; items=items; status = [status] } :>_
                | _ -> { state  with state = MainState } :>_
            | GetDirectionState _ -> 
                match unbox<Responses> choice with
                | OpenDoor (Some dir) -> 
                    let pos = GrottoCore.translatePosition state.santa.entity.loc dir
                    match state.tiles.TryFind pos with
                    | Some(Door false ) -> 
                        { state with state = MainState;status = ["the door is already open"]} :>_
                    | Some(Door true ) -> 
                        let tiles = state.tiles.Add(pos,Door false)
                        endTurn { state with state = MainState;status = ["you open the door.."]; tiles=tiles} :>_
                    | _ ->                     
                        { state with state = MainState;status = ["there is no door here."]} :>_
                | CloseDoor(Some dir)-> 
                    let pos = GrottoCore.translatePosition state.santa.entity.loc dir
                    match state.tiles.TryFind pos with
                    | Some(Door true ) -> 
                        { state with state = MainState; status = ["the door is already closed"]} :>_
                    | Some(Door false ) -> 
                        let tiles = state.tiles.Add(pos,Door true)
                        endTurn { state with state = MainState; status = ["you close the door.."]; tiles=tiles} :>_
                    | _ ->                     
                        { state with state = MainState; status = ["there is no door here."]} :>_
                | _ -> state :>_           
            | MainState -> 
                match unbox<Responses> choice with
                | More -> 
                    let status = 
                        match state.status with
                        | [] 
                        | _ :: _ :: []
                        | _ :: [] -> []
                        | _ :: _ :: z -> z
                    {state with status = status} :>_
                | Pickup -> 
                    let items, status =
                        match state.items |> List.tryFind(fun i -> i.loc = Some state.santa.entity.loc ) with
                        | Some i -> let items = state.items |> List.filter((<>)i)
                                    { i with loc = None} :: items, (sprintf "you pickup %s" i.itemType.Text)
                        | _ -> state.items, "there is no item here"
                    { state with items = items; status = [status]} :>_
                | Move(direction) -> 
                  let newPos = GrottoCore.translatePosition state.santa.entity.loc direction

                  match state.entities |> List.tryFind(fun e -> e.loc = newPos) with
                  | Some e -> 
                    // implement combat
                    let entities = state.entities|>List.filter(fun e' -> e' <> e)
                    { state with status = ["you attack!"] ; entities=entities} :>_
                  | None -> 
                    match state.tiles.TryFind newPos with
                    | None | Some HWall | Some VWall | Some (Door true) | Some Empty -> {state with state = MainState; status = ["you cannot move in that direction!"] }  :> _
                    | _ ->                       
                        let status = 
                          let temp =
                              match state.items |> List.tryFind(fun i -> i.loc = Some newPos ) with
                              | Some i -> match i.itemType with
                                          | MincedPie _ -> ["there is an unidentified minced pie here"]
                                          | Carrot Rotten -> ["there is a rotten carrot here"]
                                          | Carrot NotSoGood -> ["there is a carrot here that looks past its best"]
                                          | Carrot Amazing -> ["there is an awesome looking carrot here"]
                                          | Presents i when i = 1 -> ["there is a present here"]
                                          | Presents i -> [sprintf "there is a pile of %i presents here" i]
                              | None -> []

                          match state.tiles |> Map.tryFind( newPos) with
                          | Some(Stairs true) -> "you see stairs leading up here" :: temp
                          | Some(Stairs false) -> "you see stairs leading down here" :: temp
                          | _ -> temp

                        let fov = GrottoCore.calculateFov newPos state.tiles state.rooms
                        endTurn {state with state=MainState; status = status; discoveredTiles = Set.union state.discoveredTiles fov;currentFov=fov; santa = { state.santa with entity = {state.santa.entity with loc = newPos }} } :>_
                | OpenDoor None ->  {state with state = GetDirectionState (OpenDoor None); } :>_
                | CloseDoor None -> {state with state = GetDirectionState (CloseDoor None);} :>_
                | UseItem None -> {state with state = GetItemState (UseItem None);} :>_
                | _ -> state :>_                        
            | _ -> state :>_