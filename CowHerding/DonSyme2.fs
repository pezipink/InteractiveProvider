module DonSyme2


open InteractiveProvider.Interfaces
open System
open System.Text
open System.Reflection
open System.IO
open Cows
open FSharp.Data

type MenuTypes =
    | Introduction
    | FactSelect
    | Facts of bool
    | CollectionSelect 
    | LevelSelect of LevelCollection 
    | Game of LevelData
    with member  x.Collection = match x with LevelSelect l -> l | _ -> failwith ""
         member  x.Data = match x with Game d -> d | _ -> failwith ""

type System.String with
    member x.ValueOption() = if String.IsNullOrWhiteSpace x then None else Some x 

let wrapAndSplit (text:string) =
    let text = 
        text.Split([|Environment.NewLine|], StringSplitOptions.None)
        |> Array.map(fun s -> String.Format("<para>{0}</para>", s.Replace(" "," ")))
        |> fun t ->  String.Join("\r\n", t)
    String.Format("<summary>{0}</summary>", text)

let getFact nerdy =
    let getText req = 
        match (Http.Request req).Body with
        | Text t -> t
        | _ -> failwith "unexpected response" 
    try
        let joke =
            if nerdy then DonNorris.Parse(getText "http://api.icndb.com/jokes/random?firstName=Don&lastName=Syme&limitTo=nerdy")
            else DonNorris.Parse(getText "http://api.icndb.com/jokes/random?firstName=Don&lastName=Syme")
        joke.Value.Joke
    with 
    | ex -> ""

let formatLevel level =
    let sb = System.Text.StringBuilder()
    let (~~) (s:string) = sb.Append s |> ignore
    let (~~~) (s:string) = sb.AppendLine s |> ignore
    for y = 0 to level.Height-1 do
        for x = 0 to level.Width-1 do
            match level.Data.TryFind(y,x) with
            | Some x -> ~~(sprintf "%s" (x.ToString()))
            | None -> ~~ " " 
        ~~~ ""
    sb.ToString() |> wrapAndSplit
    
let rec play(state:MenuTypes) =
    let level = state.Data
    if levelComplete level then 
        { displayOptions = fun _ -> 
            [
                "# View Map", box false          
                "CONGRATULATIONS!", box true
            ] 
          displayText = fun _ -> 
            formatLevel level  
          processResponse = fun (s,r) -> 
            match (unbox<bool> r) with
            | false -> play state :> _
            | true ->collectionSelect() :> _                
          state = state }
    else
       { displayOptions = fun _ -> 
            [
                "# View Map", box None
                "North", box <| Some North;
                "East", box <| Some East;
                "South", box <| Some South;
                "West", box <| Some West;
            ] 
         displayText = fun _ -> 
            formatLevel level  
         processResponse = fun (s,r) -> 
            match (unbox<Direction option> r) with
            | None -> play state :> _
            | Some dir ->
                let d = moveDon dir level.Data
                play(Game {level with Data = d}) :> _
         state = state }

and levelSelect(state:MenuTypes) =
    let collection = state.Collection
    { displayOptions = fun _ ->
        [for level in collection.Levels do
            yield level.Id, box level] 
      displayText = fun _ -> 
        sprintf
            "<para>Level Collection: %s</para>
            <para>Copyright : %s</para>
            <para>Description : %s</para>
            <para>.</para>
            <para>Select a level collection from the menu. You can get more levels by dropping more .slc files into the directory.</para>"
            collection.Title
            collection.Copyright
            collection.Description
      processResponse = fun (_,level) -> play (Game (level:?>_)) :> _
      state = LevelSelect collection } 

and collectionSelect() = 
    let collections = 
        let current = FileInfo(System.Reflection.Assembly.GetCallingAssembly().Location)
        let dir = current.Directory
        Directory.GetFiles(dir.FullName)
        |> Array.map FileInfo
        |> Array.filter(fun x -> x.Extension = ".slc")
        |> Array.map(fun x -> CowLevel.Load(x.FullName) |> Cows.readLevels)
    { displayOptions = fun _ ->
        [for collection in collections do
            yield collection.Title, box collection] 
      displayText = fun x -> 
        "Select a level collection from the menu. You can get more levels by dropping more .slc files into the directory."
      processResponse = fun (e,collection) -> levelSelect(LevelSelect (collection:?>_)) :> _
      state = CollectionSelect } 

let rec factCycle factType =
    { displayOptions = fun _ ->
        ["Learn another amazing fact",box 1]
      displayText = fun _ -> getFact factType
      processResponse = fun _ -> factCycle factType :> _
      state = Facts factType }
    
let factTypeSelect() =
    { displayOptions = fun _ ->
          ["All", box false
           "Technical",box true]
      displayText = fun _ ->
       "Select a fact category"
      processResponse = fun (e,resp) ->
        factCycle (resp :?> bool) :> _
      state = FactSelect }
     

let start() = 
    { displayOptions = fun _ -> 
        ["--> Learn some facts",box 1 ;
         "--> Herd some Cambridge Cows",box 2]
      displayText = fun _ -> 
        "Welcome to the Don Syme 2.0 Type Provider, CowHerding edition.
        Choose an option to learn some facts about Don, or help him on his quest to herd the Cambridge Cows to safety."
      processResponse = fun (e,i) -> 
        match i :?> int with
        | 1 -> factTypeSelect() :> _ 
        | 2 -> collectionSelect() :> _ 
        | _ -> failwith "!" :> _
      state = Introduction } 

type DonSyme() =
    interface IInteractiveServer with
        member x.NewState: IInteractiveState = start() :> IInteractiveState        
        member x.ProcessResponse(state: IInteractiveState, response: obj): IInteractiveState =      
            let state = (state:?>InteractiveState<MenuTypes>)
            state.ProcessResponse(response) 
                                    
