namespace Xmas

open InteractiveProvider.Interfaces
open System

module Utils =
    let rnd = Random(DateTime.Now.Millisecond)    
    let rndBool() = if rnd.Next(2) = 0 then true else false
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
    with static member AllDirections = [North;NorthEast;East;SouthEast;South;SouthWest;West;NorthWest]

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
    | Teleport    
    | HealthUp
    | HealthDown
    | Food
    | Experience
    | Psy

type CarrotValue =
    | Amazing
    | Rotten
    | NotSoGood
    
type ItemType =
    | MincePie of PieType
    | Carrot of CarrotValue 
    | Presents of int            
    with 
        member this.Draw =
            match this with
            | MincePie _ -> "!"
            | Carrot _ -> "%"
            | Presents _ -> "*"        

        member this.Text =
            match this with
            | MincePie _ -> "an mince pie"
            | Carrot Rotten -> "a rotten carrot"
            | Carrot NotSoGood -> "a carrot that looks past its best"
            | Carrot Amazing -> "an awesome looking carrot"
            | Presents i when i = 1 ->"a present"
            | Presents i -> sprintf "a pile of %i presents" i

type Item =
    { itemType:ItemType
      loc:Point option
      id : Guid}
                  
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
    member this.Name =
        match this with
        | Santa ->"Evil Santa"
        | Elf ->"Elf"
        | Reindeer ->"Reindeer"
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

    let generateDungeon width height level =
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
                        (Set.add cp doors, (cp,Tile.Door (rndBool())) :: acc)
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
            [for i in 1..(D 5 (level+1)) do
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
                    if (D 1 3) = 1 then{loc=p;etype=Reindeer;active=false;health=(10+level,10+level)} 
                    else {loc=p;etype=Elf;active=false;health=(5+level,5+level)} )

        // place some items around the level
        let getRandomItem() =
            
            let r = D 1 3
            if r = 1 then 
                match D 1 8 with
                | 1 -> MincePie Healing
                | 2 -> MincePie Poison
                | 3 -> MincePie Experience
                | 4 -> MincePie Psy
                | 5 -> MincePie HealthDown
                | 6 -> MincePie HealthUp
                | 7 -> MincePie Food
                | _ -> MincePie Teleport
            elif r = 2 then
                let t = D 1 3
                if t = 1 then Carrot Amazing
                elif t = 2 then Carrot Rotten
                else Carrot NotSoGood
            else
                Presents (D 1 20)    

        let items = 
            [for i in 1..(D 5 (6-level/2)) do
                // pick a random room ..
                let room = roomRects.[rnd.Next(roomRects.Length)]
                // pick a spot in the room which isnt a wall
                let x = rnd.Next(room.Location.X + 1, room.Location.X + room.Width)
                let y = rnd.Next(room.Location.Y + 1, room.Location.Y + room.Height)
                let p = {X=x;Y=y}
                if p <> (fst stairsUp) then yield p]
            |> Set.ofList
            |> Set.toList
            |> List.map(fun p -> {loc=Some p;itemType = getRandomItem(); id = Guid.NewGuid() } )


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
        
    let relativeDirection current target =
        if   target.X > current.X && target.Y > current.Y then SouthEast
        elif target.X > current.X && target.Y = current.Y then East
        elif target.X > current.X && target.Y < current.Y then NorthEast
        elif target.X = current.X && target.Y < current.Y then North
        elif target.X < current.X && target.Y < current.Y then NorthWest
        elif target.X < current.X && target.Y = current.Y then West
        elif target.X < current.X && target.Y > current.Y then SouthWest
        elif target.X = current.X && target.Y > current.Y then South
        else failwith "wpf"
    
    let getLevel xp =
        if xp < 20 then 1
        elif xp < 40 then 2
        elif xp < 80 then 3
        elif xp < 120 then 4
        else 5

    let calculateFov (point:Point) (map:Map<Point,Tile>) (rooms:(Point*Tile) list list)=
        // a terrible fov algo 
        if map.ContainsKey point = false then Set.empty else
        let rec unfoldLine point direction count acc =
            // this functions walks in a straight line for 3 tiles unless stopped 
            // by walls or empty tiles
            if count > 3 then acc else
            let newPoint = translatePosition point direction                
            match map.TryFind newPoint with
            | None
            | Some Empty 
            | Some HWall
            | Some VWall 
            | Some (Door true) -> point :: acc // stop here but include this tile
            | _ -> unfoldLine newPoint direction (count+1) (point :: acc)
        match map.[point] with
        | Corridor 
        | Door false ->
            // for a corridor we can simply see 3 tiles n, s, e, w along it
            Direction.AllDirections
            |> List.map(fun d -> unfoldLine point d 0 [] )
            |> List.collect id
            |> Set.ofList
        | Door true
        | RoomFloor 
        | Stairs _ ->
            // just show all the room if in a room 
            let room = rooms |> List.find (fun r -> List.exists(fun (p,_)-> p = point) r)
            room |> List.map fst |> Set.ofList
        | _ -> Set.empty

    let restoreHealth amt (current,max) =
        if current + amt > max then max,max else current+amt,max
    
    let incMaxHealth amt (current,max) =
        current, max + amt

    let decMaxHealth amt (current,max) =
        let max = max - amt
        let current = if current > max then max else current
        current, max

    let damageHealth amt (current,max) = current-amt,max

    let itemAction tiles item entity =
        match item with
        | MincePie Healing -> let amt = D 1 6 in {entity with health = restoreHealth amt entity.health }
        | MincePie Poison -> let amt = D 1 6 in {entity with health = restoreHealth amt entity.health }
        | MincePie HealthUp -> let amt = D 1 6 in {entity with health = incMaxHealth amt entity.health }
        | MincePie HealthDown -> let amt = D 1 6 in {entity with health = decMaxHealth amt entity.health }
        | MincePie Teleport -> 
            let rec findPos() = 
                let x = rnd.Next width
                let y = rnd.Next width
                let p = {X=x;Y=y}
                match Map.tryFind p tiles with                
                | Some RoomFloor
                | Some Corridor -> p
                | _ -> findPos()
            let newPos = findPos()
            {entity with loc = newPos }
        // these ones have no affect on monsters
        | Carrot _ -> entity 
        | Presents _ -> entity
        | MincePie Food -> entity
        | MincePie Experience -> entity
        | MincePie Psy -> entity

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
    | Climb
    | Wait
    | DropItem of Item option

type GameState = 
    | NewState
    | MainState
    | WinState
    | LoseState
    | GetDirectionState of Responses
    | GetBoolState of Responses
    | GetItemState of Responses

type Santa =
    { entity : Entity
      hunger : int
      inventory : Item list
      lastHealTurn : int
      exp : int
      presents:int
      turnsLeftUnderTheInfluence : int}

type LevelData = {
    entities : Entity list
    tiles:Map<Point,Tile>
    discoveredTiles : Set<Point>
    rooms:(Point*Tile)list list    
    items:Item list }

type SantasGrottoState =
    { state:GameState; levelData:Map<int,LevelData>; status : string list; 
      currentFov:Set<Point>; santa:Santa; turnsElapsed : int; grottoLevel : int
      pieMap : Map<PieType,string> }
      member this.CurrentLevel = this.levelData.[this.grottoLevel]
      member this.StairsUp level = this.levelData.[level].tiles |> Map.findKey(fun k v -> match v with Stairs true -> true | _ -> false )
      member this.StairsDown level = this.levelData.[level].tiles |> Map.findKey(fun k v -> match v with Stairs false -> true | _ -> false )
      interface IInteractiveState with
            member this.DisplayText = 
                  let toText () = 
                      let eLookup = this.santa.entity :: this.CurrentLevel.entities |> List.map(fun e -> e.loc, e ) |> Map.ofList
                      let iLookup = this.CurrentLevel.items |> List.map(fun e -> e.loc, e ) |> Map.ofList
                      let sb = System.Text.StringBuilder()
                      let rndAscii() = (char(rnd.Next(46,127)))
                                       |> function
                                            | '\'' -> "&apos;"
                                            | '\"' -> "&quot;"
                                            | '&' -> "&amp;"
                                            | '<' -> "&lt;"
                                            | '>' -> "&gt;"
                                            | c -> string c
                      let drawet (et:EntityType) =
                        if this.santa.turnsLeftUnderTheInfluence > 0 then rndAscii()
                        else et.Draw
                      let drawit (it:ItemType) =
                        if this.santa.turnsLeftUnderTheInfluence > 0 then rndAscii()
                        else it.Draw
                      let drawt (t:Tile) =
                        if this.santa.turnsLeftUnderTheInfluence > 0 then rndAscii()
                        else t.Draw
                      sb.Append("<summary>") |> ignore
                      for y = 0 to GrottoCore.height do                      
                        sb.Append "<para>" |> ignore
                        for x = 0 to GrottoCore.width do
                            // display previously discovered tiles, and anything else that is currently visible.
                            // this is slow but who cares
                            let showTile p = if Set.contains p this.CurrentLevel.discoveredTiles then true else false
                            let point = {X=x;Y=y}
                            match eLookup.TryFind(point) with
                            | Some e when this.currentFov.Contains point -> sb.Append (drawet e.etype) |> ignore
                            | _ ->
                                match iLookup.TryFind(Some point) with
                                | Some e when this.currentFov.Contains point -> sb.Append (drawit e.itemType) |> ignore
                                | _ ->
                                    match this.CurrentLevel.tiles |> Map.tryFind point with
                                    | Some e when showTile point -> sb.Append (drawt e) |> ignore
                                    | _ -> sb.Append " " |> ignore
                        sb.Append "</para>" |> ignore
                      let hungry = if this.santa.hunger < 10 then "you are starving to death!"
                                   elif this.santa.hunger < 30 then "you are hungry"
                                   else ""
                      sb.AppendFormat("<para>Health:{0}({1})Exp:{2} | {3} | Presents: {4} | Grotto Level : {5} | {6}</para>",(fst this.santa.entity.health),(snd this.santa.entity.health), this.santa.exp, (GrottoCore.getLevel  this.santa.exp), this.santa.presents, this.grottoLevel, hungry   ) |> ignore 
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
                        """Welcome to the Escape From Santa's Grotto / North Pole type provider!
Created December 2014 by Ross McKinlay, pinksquirrellabs.com 
You can find the source at my github (pezipink), pull requests welcome!
Poor Saint Nick had one too many sherries and has woken up at the bottom floor of his workshop and grotto!  Unfortunately, it seems that his elves, reindeer and a host of other creatures have also been on the sherry and are rampaging around. 

Can you help jolly old Father Christmas to fight his way out and escape the grotto?

This roguelike was developed in the spirit of the original Rogue, 1980~ ! 
"""
                  | WinState -> 
                    """CONGRATULATIONS! 

                    You have managed to best the drunken hordes and escape the
                    grotto.  Christmas will go ahead like normal next year!"""
                  | LoseState -> 
                    """GAME OVER!!

                    Ultimately poor Saint Nick was bested by the drunken hordes,
                    sealing the fate of Christmas for future generations!
                    """
                  | _ -> toText() 
                  |> Utils.wrapAndSplit
            member this.DisplayOptions = 
                if this.status.Length > 2 then ["# Grotto View", Nothing :>_; "More", More :>_] else
                match this.state with
                | WinState -> [ "YOU WIN!!", Nothing :>_ ]
                | LoseState -> [ "GAME OVER!!", Nothing :>_ ]
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
                     "Pickup",(Pickup ) :>_ 
                     "Drop",  (DropItem None) :>_ 
                     "Wait",  (Wait) :>_
                     "Climb", Climb :>_]
                | GetBoolState response -> 
                    ["# Grotto View", Nothing  :>_
                     "Yes",  response :>_
                     "No",   response :>_ ]
                | GetItemState f -> 
                    let f = match f with UseItem _ -> UseItem | _ -> DropItem
                    let count = 
                        this.santa.inventory 
                        |> Seq.groupBy(fun i -> i.itemType) 
                        |> Seq.map(fun (k,v) -> k, Seq.length v ) 
                        |> Map.ofSeq
                    let items = 
                        this.santa.inventory 
                        |> List.map(fun i -> let c = if count.[i.itemType] > 1 then sprintf " (%i)" count.[i.itemType] else ""
                                             match i.itemType with 
                                             | MincePie pt -> sprintf "a %s mince pie%s" this.pieMap.[pt] c, f(Some i) :> obj
                                             | _ ->  (sprintf "%s%s" i.itemType.Text c, f(Some i) :> obj))
                                             
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
                

type ``The North Pole``() =
    let rnd = System.Random(System.DateTime.Now.Millisecond)
    
    interface IInteractiveServer with
        member this.NewState = 
            let pieMap = 
                ["Green"; "Blue"; "Red"; "Purple"; "Pink"; "Yellow"; "Normal";"Multi-Coloured"] 
                |> List.sortBy(fun _ -> rnd.Next(10)) 
                |> List.zip [Poison;Teleport;Healing;HealthUp;HealthDown;Food;Experience;Psy] |> Map.ofList
            { state = NewState; status = ["Welcome to Level 1, Saint Nick!"]; 
              currentFov = Set.empty ; levelData = Map.empty
              santa= Unchecked.defaultof<_>; turnsElapsed = 0; grottoLevel=1
              pieMap = pieMap } :> IInteractiveState
        member this.ProcessResponse(client,choice) =
            let state = client :?> SantasGrottoState            
            let endTurn state = 
                let health = if state.turnsElapsed - state.santa.lastHealTurn > 30 && (fst state.santa.entity.health) < (snd state.santa.entity.health)   then 
                                (fst state.santa.entity.health) + 1, (snd state.santa.entity.health) 
                             else state.santa.entity.health
                let health = if state.santa.hunger <= 0 then (fst health) - 3, (snd health) 
                             elif state.santa.hunger <= 10 then (fst health) - 1, (snd health)                     
                             else health
                let monsters = state.CurrentLevel.entities |> List.map( fun e -> 
                    if e.active then e
                    elif Set.contains e.loc state.currentFov then
                        if Utils.rnd.NextDouble() <= 0.3 then {e with active = true }
                        else e
                    else e)
                let monsters, status, dmg, items = 
                    (([],state.status,0,state.CurrentLevel.items),monsters)
                    ||> List.fold(fun (out,status,dmg,items) e ->
                        if e.active = false then (e::out,status,dmg,items) else
                        let dist = e.loc.DistanceTo state.santa.entity.loc 
                        if dist <= 1 then 
                            // attack!
                            if D 1 20 > (17-state.grottoLevel) then 
                                let s = if rndBool() then "bites" else "hits"
                                let d = 
                                    match e.etype with
                                    | Elf -> D 1 4
                                    | Reindeer -> D 1 6
                                    | _ -> 0
                                (e::out,(sprintf "the %s %s you %i %i" e.etype.Name s d dmg)::status,dmg+d,items)
                            else
                                (e::out,(sprintf "the %s misses you" e.etype.Name)::status,dmg,items)
                            
                        elif dist > 1 then
                            if Utils.rnd.NextDouble() <= 0.9 then
                                let dir = GrottoCore.relativeDirection e.loc state.santa.entity.loc
                                let pos = GrottoCore.translatePosition e.loc dir
                                let blocked = state.CurrentLevel.entities |> List.exists(fun e -> e.loc = pos )
                                let pos = if blocked then GrottoCore.translatePosition e.loc Direction.AllDirections.[rnd.Next(Direction.AllDirections.Length)] else pos
                                match state.CurrentLevel.tiles.TryFind pos with
                                | Some(Door false) 
                                | Some(Stairs _) 
                                | Some(Corridor) 
                                | Some(RoomFloor) when state.CurrentLevel.entities |> List.exists(fun e -> e.loc = pos ) = false ->  
                                    let e, items, status =
                                        match state.CurrentLevel.items |> List.tryFind(fun i -> i.loc =Some pos) with
                                        | Some i  when rndBool() ->                                             
                                            GrottoCore.itemAction state.CurrentLevel.tiles i.itemType e ,items|>List.filter((<>)i),(sprintf "you notice a %s consuming %s!" e.etype.Name i.itemType.Text ):: status
                                        | _ -> e,items,status
                                    ({ e with loc = pos }::out,status,dmg,items)
                                | _ -> (e::out,status,dmg,items)
                            else (e::out,status,dmg,items)
                        else (e::out,status,dmg,items)
                     )

                let health = (fst health) - dmg, snd health
                let tluti =(if state.santa.turnsLeftUnderTheInfluence > 0 then state.santa.turnsLeftUnderTheInfluence-1 else 0)
                let santa = { state.santa with hunger = state.santa.hunger - 1; turnsLeftUnderTheInfluence = tluti; entity = {state.santa.entity with health = health } }                            
                let level = {state.CurrentLevel with entities = monsters; items=items }
                if (fst santa.entity.health) < 0 then {state with state = LoseState }
                else {state with turnsElapsed = state.turnsElapsed + 1; status = status; santa = santa; levelData = state.levelData.Add(state.grottoLevel,level) }

            match state.state with
            | NewState -> 
                match unbox<Responses> choice with
                | Introduction -> state :> IInteractiveState          
                | Start ->                     
                    let dungeon, rooms, startLocation, monsters, items = DungeonGenerator.generateDungeon GrottoCore.width GrottoCore.height 1
                    let dungeon = dungeon |> Map.ofList
                    let fov = GrottoCore.calculateFov (fst startLocation) dungeon rooms
                    let levelData = [1,{ items=items; entities=monsters; tiles=dungeon; discoveredTiles=fov; rooms=rooms }] |> Map.ofList
                    let h = rnd.Next(12,15)
                    let pies =                     
                        [
                         {itemType = MincePie Teleport; loc = None; id=Guid.NewGuid()}
                         {itemType = MincePie Teleport; loc = None; id=Guid.NewGuid()}
                         {itemType = MincePie Teleport; loc = None; id=Guid.NewGuid()}
                         {itemType = MincePie Teleport; loc = None; id=Guid.NewGuid()}
                        ]
                    let santa = { entity={loc=(fst startLocation);etype=Santa;active=true;health=(h,h)} ; 
                                  hunger = 100; inventory = pies; lastHealTurn = 0; exp = 0; presents=0
                                  turnsLeftUnderTheInfluence=0 } 
                    { state with state = MainState; levelData=levelData;currentFov=fov;santa=santa} :>_
                | _ -> state :>_
            | GetItemState _ -> 
                match unbox<Responses> choice with                
                | UseItem(Some item) -> 
                    let ia = GrottoCore.itemAction state.CurrentLevel.tiles item.itemType
                    let inv = state.santa.inventory |> List.filter((<>)item)
                    let state =
                        match item.itemType with
                        | MincePie Healing -> 
                            {state with santa = {state.santa with inventory = inv; entity = ia state.santa.entity }; status = ["you feel somewhat better!"]}
                        | MincePie HealthDown -> 
                            {state with santa = {state.santa with inventory = inv; entity = ia state.santa.entity }; status = ["yuck.. you feel permenantly less healthy!"]}
                        | MincePie HealthUp -> 
                            {state with santa = {state.santa with inventory = inv; entity = ia state.santa.entity }; status = ["after eating the pie you feel stronger!"]}
                        | MincePie Poison -> 
                            {state with santa = {state.santa with inventory = inv; entity = ia state.santa.entity }; status = ["yuck! that was a bad mince pie...."]}
                        | MincePie Teleport -> 
                            let santa =  { state.santa with inventory = inv; entity = ia state.santa.entity }
                            let fov = GrottoCore.calculateFov santa.entity.loc state.CurrentLevel.tiles state.CurrentLevel.rooms
                            let level = { state.CurrentLevel with discoveredTiles = Set.union state.CurrentLevel.discoveredTiles fov;}
                            {state with santa = santa ; status =["wow! that was one crazy mince pie!"]; levelData=state.levelData.Add(state.grottoLevel,level) ;currentFov=fov;  }
                        | MincePie Psy -> 
                            let t = rnd.Next(1,31)
                            {state with santa = {state.santa with inventory = inv; turnsLeftUnderTheInfluence=t }; status = ["woah... I'm not sure that was a mince pie..."]}
                        | MincePie Experience ->                         
                            let cl = GrottoCore.getLevel state.santa.exp
                            let xp = state.santa.exp + 30
                            let cl' = GrottoCore.getLevel (state.santa.exp+xp)
                            let santa = { state.santa with exp = (state.santa.exp+xp) }
                            let status =
                                if cl' <> cl then ["an interesting pie indeed! you feel wiser..."; sprintf "welcome to level %i!" cl'] 
                                else ["an interesting pie indeed! you feel wiser..."]
                            {state with santa = {state.santa with inventory = inv; exp = state.santa.exp + 30 }; status = status }
                        | MincePie Food ->                             
                            {state with santa = {state.santa with inventory = inv; hunger = state.santa.hunger + 50}; status = ["wow! that was a fantastic mince pie! you feel much less hungry!"] }
                        | Carrot Amazing ->                             
                            {state with santa = {state.santa with inventory = inv; hunger = state.santa.hunger + 30}; status = ["you feel less hungry"] }
                        | Carrot NotSoGood -> 
                            let amt = D 5 2
                            {state with santa = {state.santa with inventory = inv; hunger = state.santa.hunger + 20}; status = ["you feel somewhat less hungry"] }
                        | Carrot Rotten -> 
                            let amt = if rndBool() then 0 else D 1 4 
                            let status = if amt = 0 then ["you feel a little less hungry"] else ["yuck!  you feel less healthy"]
                            {state with santa = {state.santa with inventory = inv; hunger = state.santa.hunger + 10; 
                                                                 entity = { state.santa.entity with health = GrottoCore.damageHealth amt state.santa.entity.health } } ; status = status }
                        | Presents _ -> state // should not be possible
                    endTurn{ state  with state = MainState; } :>_
                | DropItem(Some item) ->
                    let santa = { state.santa with inventory = state.santa.inventory |> List.filter((<>)item) } 
                    let level = {state.CurrentLevel with items = {item with loc = (Some state.santa.entity.loc)} :: state.CurrentLevel.items }
                    let s = sprintf "you drop the %s" item.itemType.Text
                    { state with santa = santa; status = s :: state.status; levelData = state.levelData.Add(state.grottoLevel,level) } :>_
                | _ -> { state  with state = MainState } :>_
            | GetDirectionState _ -> 
                match unbox<Responses> choice with
                | OpenDoor (Some dir) -> 
                    let pos = GrottoCore.translatePosition state.santa.entity.loc dir
                    match state.CurrentLevel.tiles.TryFind pos with
                    | Some(Door false ) -> 
                        { state with state = MainState;status = ["the door is already open"]} :>_
                    | Some(Door true ) -> 
                        let tiles = state.CurrentLevel.tiles.Add(pos,Door false)
                        let levelData = { state.CurrentLevel with tiles = tiles }
                        endTurn { state with state = MainState;status = ["you open the door.."]; levelData=state.levelData.Add(state.grottoLevel,levelData)} :>_
                    | _ ->                     
                        { state with state = MainState;status = ["there is no door here."]} :>_
                | CloseDoor(Some dir)-> 
                    let pos = GrottoCore.translatePosition state.santa.entity.loc dir
                    match state.CurrentLevel.tiles.TryFind pos with
                    | Some(Door true ) -> 
                        { state with state = MainState; status = ["the door is already closed"]} :>_
                    | Some(Door false ) -> 
                        let tiles = state.CurrentLevel.tiles.Add(pos,Door true)
                        let levelData = { state.CurrentLevel with tiles = tiles }
                        endTurn { state with state = MainState; status = ["you close the door.."];levelData=state.levelData.Add(state.grottoLevel,levelData)} :>_
                    | _ ->                     
                        { state with state = MainState; status = ["there is no door here."]} :>_
                | _ -> state :>_           
            | MainState -> 
                match unbox<Responses> choice with
                | Wait -> endTurn { state with status = [] } :>_
                | More -> 
                    let status = 
                        match state.status with
                        | [] 
                        | _ :: _ :: []
                        | _ :: [] -> []
                        | _ :: _ :: z -> z
                    {state with status = status} :>_
                | Climb -> 
                    match state.CurrentLevel.tiles.[state.santa.entity.loc] with
                    | Stairs true ->  
                        let newLevel = state.grottoLevel + 1
                        if newLevel = 6 then { state  with state = WinState } :>_ else
                        let levelData, newLoc, fov = 
                            match state.levelData |> Map.tryFind newLevel with
                            | Some data -> 
                                let stairs = state.StairsDown newLevel
                                let fov = GrottoCore.calculateFov stairs data.tiles data.rooms
                                state.levelData, stairs, fov
                            | None -> 
                                // create new level
                                let dungeon, rooms, startLocation, monsters, items = DungeonGenerator.generateDungeon GrottoCore.width GrottoCore.height newLevel
                                let dungeon = dungeon |> Map.ofList
                                let fov = GrottoCore.calculateFov (fst startLocation) dungeon rooms
                                let levelData = { items=items; entities=monsters; tiles=dungeon; discoveredTiles=fov; rooms=rooms }
                                state.levelData.Add(newLevel,levelData) , (fst startLocation), fov
                        
                        let e = { state.santa.entity with loc = newLoc }
                        endTurn{ state  with status = ["you ascend the stairs..."]; currentFov=fov; levelData = levelData; grottoLevel = newLevel; santa = { state.santa with entity = e}  } :>_
                    | Stairs false -> 
                        let newLevel = state.grottoLevel - 1
                        if state.grottoLevel = 1 then  { state  with status = ["you have reached the bottom of the grotto."] } :>_
                        else 
                        let e = { state.santa.entity with loc = state.StairsUp newLevel }
                        endTurn{ state  with status = ["you descend the stairs."]; grottoLevel = newLevel; santa = { state.santa with entity = e }  } :>_
                    | _ -> { state  with status = ["you see nothing to climb here"] } :>_
                | Pickup -> 
                    let items, santa, status =
                        match state.CurrentLevel.items |> List.tryFind(fun i -> i.loc = Some state.santa.entity.loc ) with
                        | Some i -> let items = state.CurrentLevel.items |> List.filter((<>)i)
                                    let santa = 
                                        match i.itemType with
                                        | Presents x -> { state.santa with presents = state.santa.presents + x }
                                        | _ -> { state.santa with inventory = { i with loc = None} :: state.santa.inventory }
                                    items, santa, (sprintf "you pickup %s" i.itemType.Text)
                        | _ -> state.CurrentLevel.items, state.santa, "there is no item here"
                    let level = { state.CurrentLevel with items = items}
                    endTurn{ state with levelData = state.levelData.Add(state.grottoLevel,level) ; status = [status]; santa=santa} :>_
                | Move(direction) -> 
                  let newPos = GrottoCore.translatePosition state.santa.entity.loc direction

                  match state.CurrentLevel.entities |> List.tryFind(fun e -> e.loc = newPos) with
                  | Some e -> 
                    // implement combat
                    if D 1 20 > (16-(GrottoCore.getLevel state.santa.exp)) then
                        let dmg = D 1 6 + (GrottoCore.getLevel state.santa.exp)
                        let h = (fst e.health ) - dmg
                        if h >= 0 then 
                            let entities = {e with health = h, snd e.health; active = true } :: (state.CurrentLevel.entities|>List.filter(fun e' -> e' <> e))
                            let level = { state.CurrentLevel with entities = entities}
                            endTurn { state with status = [sprintf "you hit the %s!" e.etype.Name] ; levelData = state.levelData.Add(state.grottoLevel,level)} :>_
                        else
                            let entities = state.CurrentLevel.entities|>List.filter(fun e' -> e' <> e)
                            let level = { state.CurrentLevel with entities = entities}
                            let cl = GrottoCore.getLevel state.santa.exp
                            let xp = match e.etype with
                                     | Reindeer -> 6 + state.grottoLevel
                                     | Elf -> 4 + state.grottoLevel
                                     | _ -> 0
                            let cl' = GrottoCore.getLevel (state.santa.exp+xp)
                            let santa = { state.santa with exp = (state.santa.exp+xp) }
                            if cl' <> cl then 
                                let santa = {santa with entity = { santa.entity with health = fst santa.entity.health, (snd santa.entity.health) + 6 }}
                                endTurn { state with status = [sprintf "you destroy the %s." e.etype.Name;sprintf "welcome to level %i!" cl'] ; levelData = state.levelData.Add(state.grottoLevel,level); santa = santa} :>_
                            else
                                endTurn { state with status = [sprintf "you destroy the %s." e.etype.Name] ; levelData = state.levelData.Add(state.grottoLevel,level); santa = santa} :>_
                    else

                    let entities = {e with active = true } :: (state.CurrentLevel.entities|>List.filter(fun e' -> e' <> e))
                    let level = { state.CurrentLevel with entities = entities}
                    endTurn { state with status = [sprintf "you miss the %s" e.etype.Name] ; levelData = state.levelData.Add(state.grottoLevel,level)} :>_
                  | None -> 
                    match state.CurrentLevel.tiles.TryFind newPos with
                    | None | Some HWall | Some VWall | Some (Door true) | Some Empty -> {state with state = MainState; status = ["you cannot move in that direction!"] }  :> _
                    | _ ->                       
                        let status = 
                          let temp =
                              match state.CurrentLevel.items |> List.tryFind(fun i -> i.loc = Some newPos ) with
                              | Some i -> match i.itemType with
                                          | MincePie pt -> [ sprintf "there is a %s mince pie here" state.pieMap.[pt]]
                                          | Carrot Rotten -> ["there is a rotten carrot here"]
                                          | Carrot NotSoGood -> ["there is a carrot here that looks past its best"]
                                          | Carrot Amazing -> ["there is an awesome looking carrot here"]
                                          | Presents i when i = 1 -> ["there is a present here"]
                                          | Presents i -> [sprintf "there is a pile of %i presents here" i]
                              | None -> []

                          match state.CurrentLevel.tiles |> Map.tryFind( newPos) with
                          | Some(Stairs true) -> "you see stairs leading up here" :: temp
                          | Some(Stairs false) -> "you see stairs leading down here" :: temp
                          | _ -> temp

                        let fov = GrottoCore.calculateFov newPos state.CurrentLevel.tiles state.CurrentLevel.rooms
                        let level = { state.CurrentLevel with discoveredTiles = Set.union state.CurrentLevel.discoveredTiles fov;}

                        endTurn {state with state=MainState; status = status; levelData=state.levelData.Add(state.grottoLevel,level) ;currentFov=fov; santa = { state.santa with entity = {state.santa.entity with loc = newPos }} } :>_
                | OpenDoor None ->  {state with state = GetDirectionState (OpenDoor None); } :>_
                | CloseDoor None -> {state with state = GetDirectionState (CloseDoor None);} :>_
                | UseItem None -> {state with state = GetItemState (UseItem None);} :>_
                | DropItem None -> {state with state = GetItemState (DropItem None);} :>_
                | _ -> state :>_                        
            | _ -> state :>_