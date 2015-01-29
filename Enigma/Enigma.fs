module Enigma

open InteractiveProvider.Interfaces
open System
open System.Text
open EnigmaCore

[<AutoOpen>]
module Utils =
    type System.String with
        member x.ValueOption() = if String.IsNullOrWhiteSpace x then None else Some x 
    let rnd =  System.Random(System.DateTime.Now.Millisecond)
    let wrapAndSplit (text:string) =
        let text = 
            text.Split([|Environment.NewLine|], StringSplitOptions.None)
            |> Array.map(fun s -> String.Format("<para>{0}</para>", s.Replace(" "," ")))
            |> fun t ->  String.Join("\r\n", t)
        String.Format("<summary>{0}</summary>", text)


type MachineRotor =
    | LeftRotor
    | MiddleRotor
    | RightRotor


type InteractiveState<'a> =
    { displayOptions : 'a -> (string * obj) list 
      displayText : 'a -> string 
      processResponse : 'a * obj -> IInteractiveState
      state : 'a }
    member x.ProcessResponse o = x.processResponse (x.state, o)
    interface IInteractiveState with
        member x.DisplayOptions =  x.displayOptions x.state
        member x.DisplayText = x.displayText x.state

type MainMenuResponses =
    | Nothing
    | SelectLeftRotor
    | SelectMiddleRotor
    | SelectRightRotor
    | SelectReflector
    | SetWheelPosition
    | SetRingPosition
    | CreatePlugMapping
    | Translate

type EnigmaTypes =
    | Core of EnigmaCore.Enigma 
    | Strings of string
    | Rotors of MachineRotor
    with 
        member x.Enigma = match x with Core e -> e | _ -> failwith ""
        member x.String = match x with Strings s -> s | _ -> failwith ""
        member x.Rotor = match x with Rotors r -> r | _ -> failwith ""

let rec enterText(state:string,options,genDisplayText,continuation,repeatCondition) = 
    { displayOptions = fun _ -> options
      displayText = fun d -> genDisplayText d
      processResponse = fun (current:EnigmaTypes,c) -> 
        let s =  current.String + string c
        if repeatCondition s then enterText(s,options,genDisplayText,continuation,repeatCondition) :> _
        else continuation s
      state = Strings state } 

let selectMachineRotor(continutation) =
    { displayOptions = fun _ -> 
        ["Left Wheel", box LeftRotor
         "Middle Wheel", box MiddleRotor
         "Right Wheel", box RightRotor]
      displayText = fun d -> "Select a rotor."
      processResponse = fun (current,c) -> 
        continutation (c:?>MachineRotor)
      state = Rotors MachineRotor.LeftRotor } 

let showTranslation(text,continutation) =
    { displayOptions = fun _ -> 
        ["Back to menu", box ""]
      displayText = fun d -> "The translated text : " + text
      processResponse = fun (current,c) -> 
        continutation ()
      state = Strings text} 

let rec mainMenu(enigma:EnigmaTypes) = 
    let printMachine (e:EnigmaCore.Enigma) =
        let sb = StringBuilder()
        // .. code here prints stuff about the machine
        let getws (WheelPosition c) = c |> string
        let getrs (RingSetting c) = c |> string
        let getrf (Reflector c) = c |> string
        let getl (PlugBoard l) = l
        let (~~) s = sb.AppendLine s |> ignore
        ~~ ("Your Enigma machine's current configuration :")
        ~~ ("Left Rotor : ")
        ~~ ("    Mapping : " + ((fst e.Left).Mapping))
        ~~ ("    Wheel Setting : " + ((snd e.Left) |> getws ))
        ~~ ("    Ring Setting : " + ((fst e.Left).RingSetting |> getrs))
        ~~ (" ")
        ~~ ("Middle Rotor : ")
        ~~ ("    Mapping : " + ((fst e.Middle).Mapping))
        ~~ ("    Wheel Setting : " + ((snd e.Middle) |> getws ))
        ~~ ("    Ring Setting : " + ((fst e.Middle).RingSetting|> getrs ))
        ~~ (" ")
        ~~ ("Right Rotor : ")
        ~~ ("    Mapping : " + ((fst e.Right).Mapping))
        ~~ ("    Wheel Setting : " + ((snd e.Right) |> getws ))
        ~~ ("    Ring Setting : " + ((fst e.Right).RingSetting|> getrs ))
        ~~ (" ")
        ~~ ("Reflector : " + getrf e.Reflector )
        ~~ (" ")
        ~~ ("Plug Board Mappings :")
        ~~ ("    "+ (String.Join(" ", getl e.Plugboard)))
        sb.ToString() |> wrapAndSplit


    { displayOptions = fun _ -> ["# ",box Nothing;
                                 "Select a new left rotor",box SelectLeftRotor
                                 "Select a new middle rotor",box SelectMiddleRotor
                                 "Select a new right rotor",box SelectRightRotor
                                 "Select a new reflector",box SelectReflector
                                 "Adjust a rotor's wheel setting ",box SetWheelPosition
                                 "Adjust a rotor's ring setting ",box SetRingPosition
                                 "Create a plug board mapping", box CreatePlugMapping
                                 "Translate some text!", box Translate
                                 ]
      displayText = fun _ -> printMachine enigma.Enigma
      processResponse = fun (e,r) ->
        let getRotor s = 
            if s = "1" then Components.Rotor1
            elif s = "2" then Components.Rotor2
            elif s = "3" then Components.Rotor3
            elif s = "4" then Components.Rotor4
            elif s = "5" then Components.Rotor5
            elif s = "6" then Components.Rotor6
            elif s = "7" then Components.Rotor7
            else Components.Rotor8
            
        match unbox<MainMenuResponses> r with
        | Nothing -> mainMenu(e) 
        | SelectLeftRotor -> 
            enterText("",[for i in 1..8-> sprintf "Rotor %i" i, box (string i)],
                (fun s -> "Choose the rotor to place on the left"),
                (fun s -> 
                    let e = { e.Enigma with Left = getRotor s , WheelPosition 'A' }
                    mainMenu(Core e) :> IInteractiveState),
                (fun _ -> false) )
        | SelectMiddleRotor -> 
            enterText("",[for i in 1..8-> sprintf "Rotor %i" i, box (string i)],
                (fun s -> "Choose the rotor to place in the middle"),
                (fun s -> 
                    let e = { e.Enigma with Middle = getRotor s, WheelPosition 'A' }
                    mainMenu(Core e) :> IInteractiveState 
                    ),
                (fun _ -> false) )
        | SelectRightRotor -> 
            enterText("",[for i in 1..8-> sprintf "Rotor %i" i, box (string i)],
                (fun s -> "Choose the rotor to place on the right"),
                (fun s ->                     
                    let e = { e.Enigma with Right = getRotor s, WheelPosition 'A' }
                    mainMenu(Core e) :> IInteractiveState ),
                (fun _ -> false))
        | SelectReflector -> 
            enterText("",[for i in 'A'..'B' -> sprintf "Reflector %c" i, box (string i)],
                (fun s -> "Choose a reflector"),
                (fun s -> 
                    let e = { e.Enigma with Reflector = Reflector s}
                    mainMenu(Core e) :> IInteractiveState 
                    ),
                (fun _ -> false) )
        | SetWheelPosition -> 
            let apply l  = function
                | LeftRotor -> { e.Enigma with Left = (fst e.Enigma.Left, l)  }
                | MiddleRotor -> { e.Enigma with Middle = (fst e.Enigma.Middle, l)  }
                | RightRotor -> { e.Enigma with Right = (fst e.Enigma.Right, l)  }

            selectMachineRotor(fun rotor ->                  
                enterText("",[for i in 'A'..'Z' -> sprintf "%c" i, box (string i)],
                    (fun s -> "Choose a letter"),
                    (fun s -> 
                        let e = apply (WheelPosition s.[0]) rotor
                        mainMenu(Core e) :> IInteractiveState ),
                    (fun _ -> false) ) :> IInteractiveState )
        | SetRingPosition -> 
            let apply l  = function
                | LeftRotor -> { e.Enigma with Left = { fst e.Enigma.Left with RingSetting = l }, snd e.Enigma.Left }
                | MiddleRotor -> { e.Enigma with Middle = { fst e.Enigma.Middle with RingSetting = l }, snd e.Enigma.Middle }
                | RightRotor -> { e.Enigma with Right = { fst e.Enigma.Right with RingSetting = l }, snd e.Enigma.Right }

            selectMachineRotor(fun rotor ->                  
                enterText("",[for i in 'A'..'Z' -> sprintf "%c" i, box (string i)],
                    (fun s -> "Choose a letter"),
                    (fun s -> 
                        let e = apply (RingSetting s.[0]) rotor
                        mainMenu(Core e) :> IInteractiveState),
                    (fun _ -> false) ) :> IInteractiveState )
        | CreatePlugMapping -> 
            let getList (PlugBoard s) = s
            enterText("",[for i in 'A'..'Z' -> sprintf "%c" i, box (string i)],
                (fun s -> "Choose a pair of letters to map"),
                (fun s -> 
                    let e = { e.Enigma with Plugboard = PlugBoard <| s::(getList e.Enigma.Plugboard)  }
                    mainMenu(Core e) :> IInteractiveState 
                    ),
                (fun s -> s.Length < 2) )

        | Translate -> 
            enterText("",("[End]",box ("[End]")) :: [for i in 'A'..'Z' -> sprintf "%c" i, box (string i)],
                (fun s -> "Current input : " + (s.String.Replace("[End]","")) ),
                (fun s -> 
                    let s = s.Replace("[End]","")
                    let translated = translate s e.Enigma
                    showTranslation(translated,fun _ -> mainMenu(e) :> _) :> _
                    ),
                (fun s -> s.Contains("[End]") |> not) )

        |> fun x -> x :> _
      state = enigma }    : InteractiveState<EnigmaTypes>
    
let start() = 
    { displayOptions = fun _ -> ["Begin!",box ()]
      displayText = fun _ -> "Welcome to the type provider Enigma machine!"
      processResponse = fun (e,_) -> mainMenu(e) :> _
      state = Core defaultEnigma } 


type Enigma() =
    interface IInteractiveServer with
        member x.NewState: IInteractiveState = start() :> IInteractiveState        
        member x.ProcessResponse(state: IInteractiveState, response: obj): IInteractiveState =      
            let state = (state:?>InteractiveState<EnigmaTypes>)
            state.ProcessResponse(response) 
                                    