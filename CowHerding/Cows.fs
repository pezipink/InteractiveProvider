module Cows
open System

type Cambridge =
    | Cow
    | Field
    | Don of bool    // true if on a field
    | Shed of bool  // true if cow is on the Shed
    | Wall
    with override x.ToString() =
            match x with
            | Cow -> "C"
            | Field -> "░"
            | Don _ -> "D"
            | Shed true -> "*"
            | Shed false -> "۩"
            | Wall -> "█"   

type Direction = 
    | North
    | South
    | East
    | West

type Location = int * int

type LevelData = 
    { Id : string
      Width : int
      Height : int
      Data : Map<int*int, Cambridge> }

type LevelCollection =
    { Title : string
      Description : string
      Copyright : string 
      Levels : LevelData list }
    
let readLevels (root:CowLevel.SokobanLevels) =
    // reads an entire .slc sokoban level collection 
    // do we care about memory? of course not!
    
    { Title = root.Title
      Description = root.Description
      Copyright = root.LevelCollection.Copyright
      Levels = 
        root.LevelCollection.Levels
        |> Array.map(fun level -> 
            { Id = level.Id
              Height = level.Height
              Width = level.Width
              Data = 
                [for row = 0 to level.Ls.Length-1 do
                    let chars = level.Ls.[row].ToCharArray()
                    for col = 0 to chars.Length-1 do
                        let c = 
                            match chars.[col] with
                            | ' ' -> Field
                            | '#' -> Wall
                            | '$' -> Cow
                            | '.' -> Shed false
                            | '*' -> Shed true
                            | '@' -> Don false
                            | '+' -> Don true
                            | c -> failwithf  "unexpected character '%c'" c
                        yield (row,col),c
                    ]
                |> Map.ofList })
        |> Seq.toList }

let testLevels =
 CowLevel.GetSample() |> readLevels

let moveDon direction map =
    let (dx,dy),don = Map.pick(fun k v -> match v with Don _ -> Some(k,v) | _ -> None) map
    // Don can only ever be standing on a open field or a dirt tile
    let oldTile = match don with Don true -> Shed false | _ -> Field
    match direction with
    | North -> [-1,0;-2,0] 
    | South -> [1,0;2,0]
    | East  -> [0,1;0,2]
    | West  -> [0,-1;0,-2]
    |> List.choose(fun (x,y) -> 
        Map.tryPick(fun k v -> if k = (dx+x,dy+y) then Some(k,v) else None) map)
    |> function
    // can probably make some of this nicer with some APs, but whatevs
    // normal movement cases
    // Don can always move onto a dirt tile 
    | [(x,y),Field] 
    | [(x,y),Field;_] -> 
        map
        |> Map.add (x,y) (Don false)
        |> Map.add (dx,dy) oldTile
        
    // same as above, for moving onto a field without a cow in it
    | [(x,y),Shed false] 
    | [(x,y),Shed false;_] -> 
        map
        |> Map.add (dx,dy) oldTile
        |> Map.add (x,y) (Don true)
        
    // Valid cow cases. We can move a cow forward if there is an empty space behind them.
    // Shed true is also a cow but must then be replaced with a Don true
    | [(x,y),Cow; (x',y'),Field] -> 
        map
        |> Map.add (dx,dy) (oldTile)
        |> Map.add (x',y') Cow
        |> Map.add (x,y) (Don false)
        
    // Moving a cow from a field onto dirt
    | [(x,y),Shed true; (x',y'),Field] -> 
        map
        |> Map.add (dx,dy) (oldTile)
        |> Map.add (x',y') Cow
        |> Map.add (x,y) (Don true)
        
    // moving a cow from a field or dirt to a new field
    | [(x,y),Shed true; (x',y'),Shed false] 
    | [(x,y),Cow;        (x',y'),Shed false] -> 
        map
        |> Map.add (dx,dy) (oldTile)
        |> Map.add (x',y') (Shed true)
        |> Map.add (x,y) (Don true)
            
    | _ -> map
    
let printLevel level =
    for y = 0 to level.Height-1 do
        for x = 0 to level.Width-1 do
            match level.Data.TryFind(y,x) with
            | Some x -> printf "%s" (x.ToString())
            | None -> printf " "
        printfn ""      

let levelComplete level =
    level.Data
    |> Map.filter(fun _ v ->
        match v with
        | Shed false -> true
        | _ -> false)
    |> fun x -> x.Count = 0

let movements original moves =
    let data = (original.Data,moves) ||> List.fold(fun map move -> moveDon move map)
    {original with Data = data}


