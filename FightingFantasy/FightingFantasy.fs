// THIS IS UNFINISHED AND NOT REALLY PLAYABLE!!
namespace FightingFantasy

open System
open FSharp.Data.Sql
open InteractiveProvider.Interfaces

module Utils =
    [<Literal>]
    let ResolutionPath = @"F:\GIT\InteractiveProvider\packages\System.Data.SQLite.x86.1.0.90.0\lib\net451"
    let wrapAndSplit (text:string) =
        let text = 
            text.Split([|Environment.NewLine;"\n"|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map(fun s -> String.Format("<para>{0}</para><para>.</para>", s.Replace(" "," ").Replace("-","~")))
            |> fun t ->  String.Join("\r\n", t)
        String.Format("<summary>{0}</summary>", text)

type GameDB = SqlDataProvider< @"Data Source=F:\GIT\InteractiveProvider\FightingFantasy\data\FightingFantasy.sqlite ;Version=3", Common.DatabaseProviderTypes.SQLITE, Utils.ResolutionPath, 1000 >

type Dice() =
    let rng = System.Random(System.DateTime.Now.Millisecond)
    with 
        member this.D6() = rng.Next(1,7)
        member this.``2D6``() = this.D6() + this.D6()


type Attribute = 
    { Max : int
      Current : int}

// a load of type aliases for that F# loveliness
type dbEncounter = GameDB.SqlService.``[main].[Encounters]Entity``
type dbGame = GameDB.SqlService.``[main].[Games]Entity``
type dbItem = GameDB.SqlService.``[main].[Items]Entity``
type dbReference = GameDB.SqlService.``[main].[ReferenceText]Entity``
type dbReferenceOption = GameDB.SqlService.``[main].[ReferenceOptions]Entity``

type CanEscape = bool
type CombatRound = int
type ReferenceKey = string
type Currency = int
type AttributeName = string
type GameName = string
type GameId = int64

type ItemType =
    | Backpack
    | Weapon
    | Armour
    | Provisions
    | CodeWord // not really an item 
    | Currency
    | Other
    with
        static member FromString(s:String) =
            let s = s.ToLower()
            match s with
            | "backpack" -> Backpack
            | "weapon" -> Weapon
            | "armour" -> Armour
            | "provisions" -> Provisions
            | "codeword" -> CodeWord
            | "currency" -> Currency
            | "other" -> Other
            | _ -> failwithf "unknown item type %s" s

type Item =
    { Name : string
      Quantity : int
      Type : ItemType }

type Player =
    { // attribute map because some games have additional attributes
      Attributes : Map<AttributeName,Attribute> 
      Items : Item list
      // Note that current weapon and armour are removed from the items list whilst in use
      Weapon : Item option
      Armour : Item option }
    with 
        // all games have at least these three attributes as they form the core of FF games
        member p.SKILL = p.Attributes.["SKILL"]
        member p.LUCK = p.Attributes.["LUCK"]
        member p.STAMINA = p.Attributes.["STAMINA"]
        member p.HasBackpack = p.Items |> List.tryFind(fun i -> i.Type = Backpack)
        member p.Currency = 
            p.Items 
            |> List.tryPick(fun i -> if i.Type = Currency then Some i.Quantity else None )
            |> function Some v -> v | None -> 0

type Monster =
    { Name : string
      Skill : int
      Stamina : int }

// an encounter in a location can consist of the following
// a) a single monster which may or may not be escapable
// b) many monsters which may or may not be escapable, fought in series
// c) many monsters which may or may not be escapable and are fought at the same time
// the encounter may consist of one of more stages containing the above at any stage
// the results of the encoutner are therefore possibly escape, win, lose, or special.
// speical is becuase some encounters end after a set amount of turns

type EncounterResults =
    { Win     : ReferenceKey
      Lose    : ReferenceKey
      Escape  : ReferenceKey option
      Special : (ReferenceKey * int) option }

type EncounterType =
    | Single of Monster * EncounterResults 
    | Series of (Monster list) * EncounterResults 
    | Parallel of (Monster list) * EncounterResults 

type Encounter =
    { RemainingEncounters : EncounterType list
      CurrentEncounter : EncounterType option
      RoundNumber : int }
      with 
        member this.IsStarting = this.RoundNumber = 0

type ReferenceType =
    | EncounterReference of Encounter
    | NormalReference of (ReferenceKey * string) list

// describes a location
type Reference =
    { Key : ReferenceKey
      Text : string
      Modifiers : (AttributeName * int) list // this is for any special adjustments to make to the player upon entering this location eg -2 STAMINA
      Items : (Item * Currency option) list // items on the floor (auto picked up), or in a shop and their cost if so
      Type : ReferenceType }

type GlobalState =
    { Player : Player
      Dice : Dice
      References : Map<ReferenceKey,Reference>
      PreviousReference : ReferenceKey
      CurrentReference : ReferenceKey
      ShowResult : bool  } // this is used to hide the page text from the user until they press the . and look at the # property

type GameState =
    | SelectGame of GameDB.SqlService.``[main].[Games]Entity`` list
    | GeneralInstructions
    | Normal of GlobalState                 // used in normal game play, anything other than fighting
    | ViewStats of GlobalState              // when in special mode to show inventory, etc
    | Fighting of Encounter * GlobalState   // whilst in fights
    interface IInteractiveState with
        member this.DisplayText =
            match this with
            | SelectGame _ ->
                """Welcome to the Fighting Fantasy F# type provider!
                Created May 2014 by Ross McKinlay, pinksquirrellabs.com 
                You can find the source at my github (pezipink), pull requests welcome!
                .
                Gamebooks are the property of their respective authors and were sourced from ww.ffproject.com
                .
                .
                Please choose a game from the list below to begin a new adventure!"""
            | ViewStats state -> 
                sprintf
                    """Statistics and Inventory :
                    
                    Attributes  (current / max)
                    SKILL   (%i / %i)
                    STAMINA (%i / %i) 
                    LUCK    (%i / %i)
                    
                    Currently equipped weapon : %s
                    
                    Currently equipped armour : %s
                    
                    Inventory : """
                     state.Player.SKILL.Current  state.Player.SKILL.Max
                     state.Player.STAMINA.Current  state.Player.STAMINA.Max
                     state.Player.LUCK.Current  state.Player.LUCK.Max
                     (match state.Player.Weapon with None -> "None" | Some w -> w.Name)
                     (match state.Player.Armour with None -> "None" | Some a -> a.Name)
                     
            | Normal state -> 
                if state.ShowResult = false then "Continue.."
                else 
                    let reference = state.References.[state.CurrentReference]
                    reference.Text
            | _ -> "!"
            |> Utils.wrapAndSplit
        member this.DisplayOptions = 
            match this with
            | SelectGame games -> 
                ("# Fighting Fantasy Instructions", box <| int64 -1) 
                ::  (games |> List.map(fun g -> sprintf "%s by %s" g.game_name g.game_author, box g.game_id ))
            | ViewStats state -> [("# Continue", box () )]
            | Normal state -> 
                let reference = state.References.[state.CurrentReference]
                ("#", box <| "#")
                ::  ("# View Stats", box <| "#VIEWSTATS")
                // the options here will depend on the reference type
                // if its a normal reference, display all the otions
                // otherwise, display the option to fight (enter encounter)
                :: (match reference.Type with 
                    | NormalReference data -> data |> List.map(fun (k,v) -> v, box k)
                    | EncounterReference _ -> ["Enter Combat!",box "#COMBAT"])
            | _ -> []

type FightingFantasy() =
    let ctx = GameDB.GetDataContext()

    /// transforms db encounters for a reference into a compatible encounter type
    let loadEncounters (encounters:dbEncounter list)  =
        let createMonster (e:dbEncounter) = 
            {Name = e.creature_name; Skill = int e.creature_skill; Stamina = int e.creature_stamina }
        let createResults (e:dbEncounter) = 
            {Win = e.win_reference; Lose = e.lose_reference; 
             Escape = if String.IsNullOrWhiteSpace(e.escape_reference) then None else Some e.escape_reference 
             Special = if String.IsNullOrWhiteSpace(e.special_reference) then None else Some(e.special_reference, int e.special_round)  }
        // each reference may have several groups of creatures to fight, organised into 
        // what I call phases
        let encounters = 
            encounters
            |> Seq.groupBy(fun e -> e.phase)
            |> Seq.map(fun (key, values) -> 
                let values = values |> Seq.toList
                let hasSpecial = not <| String.IsNullOrWhiteSpace(values.[0].special_reference) 
                if values.Length = 1 then Single(createMonster values.[0], createResults values.[0])
                // false = "Serial" true = "Parallel"
                elif values.[0].phase_type then Series(List.map createMonster values, createResults values.[0])
                else Parallel(List.map createMonster values, createResults values.[0]))
            |> Seq.toList

        { RemainingEncounters = encounters; CurrentEncounter = None; RoundNumber = 0}
    
    let loadItems (items:dbItem list) = 
        items |> List.map(fun item -> 
            let i = 
                {Name = item.item_name 
                 Quantity = 0 // item.
                 Type = ItemType.FromString item.item_type }
            if item.item_cost > 0L then (i, Some (int item.item_cost))
            else (i, None))
        
    let loadReferences (rs:dbReference list) = 
        rs |> List.map(fun r -> 
            { Key = r.reference_key
              Text = r.reference_text
              Modifiers = [] // r.modifications  // todo - this needs parsing etc 
              Items = loadItems (Seq.toList r.FK_Items_0_0)
              Type = 
                let e = Seq.toList r.FK_Encounters_0_0
                if e.Length = 0 then NormalReference(r.FK_ReferenceOptions_0_0 
                                     |> Seq.toList 
                                     |> List.map(fun ro -> ro.target_reference_id, ro.option_text))
                else EncounterReference (loadEncounters e) })

    let loadGame gameId =
        let game = query{ for g in ctx.``[main].[Games]`` do 
                          where (g.game_id = gameId)
                          select g 
                          head }

        let refs = game.FK_ReferenceText_0_0 |> Seq.toList |> loadReferences
        let dice = Dice()
        let createAttribute value ={ Max = value; Current = value}
        let player = 
            { Attributes = [("SKILL", createAttribute(dice.D6() + 6));
                            ("STAMINA", createAttribute(dice.D6() + 18)); // todo: starting stats and items vary between games
                            ("LUCK", createAttribute(dice.D6() + 6));]
                           |> Map.ofList 
              Items = []
              Weapon = Some {Name="Sword"; Quantity = 1; Type = Weapon}
              Armour = Some {Name="Leather Clothes"; Quantity = 1; Type = Armour} }
      
        let refLookup = 
            refs
            |> List.map(fun r -> r.Key,r)
            |> Map.ofList

        { Player = player
          Dice = dice
          References = refLookup
          CurrentReference = "START"
          PreviousReference = "START"
          ShowResult=false }
                   
    interface IInteractiveServer  with
        member this.NewState = 
            let games = ctx.``[main].[Games]`` |> Seq.toList
            SelectGame games :> IInteractiveState

        member this.ProcessResponse(state,choice) =
            match state :?> GameState with
            | SelectGame prev ->  
                let choice = unbox<int64> choice
                // load game
                if choice = -1L then state else
                Normal(loadGame choice) :> IInteractiveState
            | Normal prev -> 
                // choice will be either a reference or one of the special tags
                let choice = unbox<string> choice
                match choice with
                | "#" -> Normal({prev with PreviousReference = prev.CurrentReference; ShowResult=true}) :> IInteractiveState
                | "#VIEWSTATS" -> ViewStats(prev) :> IInteractiveState
                | newReference -> 
                    let newState = {prev with CurrentReference = choice }
                    // todo: if this is a new state (its changed) then 
                    // we need to add any modifiers and items that are not in a shop
                    Normal(newState) :> IInteractiveState
            | ViewStats prev -> Normal(prev) :> IInteractiveState
                
            | _ -> state
            