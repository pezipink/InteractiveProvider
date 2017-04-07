module RogueOneTP
open InteractiveProvider
open InteractiveProvider.Interfaces

let rnd = System.Random(System.DateTime.Now.Millisecond)
    
type pos = int * int
type RogueOneState =
     | Introduction
     | Game of int64 * pos * pos
     | Win        

let height = 10
let width = 10
let getRandomPos() =
    (rnd.Next(0,width),rnd.Next(0,height))

let getGoodPos badPos = 
    let rec aux() =
        let p = getRandomPos()
        if badPos <> p then p
        else aux()
    aux()

let createState() =
    let disk = getRandomPos()
    let crane = getGoodPos disk
    Game(System.DateTime.Now.Ticks,crane,disk)
    
let updateLevel (Game(ticks,(cx,cy),(dx,dy)))  input tickDelay =
    let (cx',cy') =
        match input with
        | 1 -> (cx,cy-1)
        | 2 -> (cx,cy+1)
        | 3 -> (cx-1, cy)
        | 4 -> (cx+1, cy)
    if (cx',cy') = (dx,dy) then Win
    else
        let newPos = 
            if cx' > width || cx' < 0 || cy' > height || cy' < 0 then
                cx,cy
              else 
                cx',cy'
        let dx',dy',ticks' =
            if System.DateTime.Now.Ticks- ticks > tickDelay then
                let (x,y) = getGoodPos(newPos)
                (x,y,System.DateTime.Now.Ticks)
            else dx,dy,ticks
        Game(ticks',(cx',cy'),(dx',dy'))

let formatLevel (Game(ticks,crane,disk)) delay=
    let sb = System.Text.StringBuilder()
    sb.AppendLine("Current delay: " + (string delay)) |> ignore
    let (~~) (s:string) = sb.Append s |> ignore
    let (~~~) (s:string) = sb.AppendLine s |> ignore
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            if (x,y) = crane then ~~ "C"
            elif (x,y) = disk then ~~ "D"
            else ~~ "."             
        ~~~ ""
    sb.ToString() |> wrapAndSplit

let rec win() =
    {displayOptions = fun _ -> []
     displayText = fun _ -> 
        "Congratualtions! You have byassed the legacy enterprise database and recovered the disk for the Rebellion!"
     processResponse = fun _ -> win() :> _
     state = Win }
        
let rec game(state:RogueOneState, delay) =
    { displayOptions = fun _ -> 
        ["Up", box 1
         "Down", box 2
         "Left", box 3
         "Right", box 4]
      displayText = fun _ -> formatLevel state delay
      processResponse = fun (e,i) ->
            match updateLevel state (i:?>int) delay with
            | Game(_,_,_) as g -> game (g, delay) :> _
            | Win -> win() :> _
      state = state 
    }

let start() = 
    
    { displayOptions = fun _ -> 
        [("Slow", box 5000000L);
         ("Faster", box 900000L);
         ("Ross", box 500000L) ;]
      displayText = fun _ -> 
        "Welcome to the Rogue One Type Provider!
   
   |                   |  
   |                   |  
   |                   |  
   |        /^\        |    
   ||===I||(-@-)||I===||
   |        \_/        |  
   |                   |
   |                   |
   |                   |
   |                   |
   |                   |

        Help overcome the Empire's legacy database system with the power of the force and F# type providers!
        Using this type provider you can access the remote database controls, but can you operate the crane to collect the disk quick enough?"
        |> wrapAndSplit
      processResponse = fun (e,i) -> 
        match i :?> int64 with
        | 1L -> game(createState(), 900000L) :> _ 
        | delay -> game(createState(), delay) :> _
      state = Introduction } 

type RogueOne() =
    interface IInteractiveServer with
        member x.NewState: IInteractiveState = start() :> IInteractiveState        
        member x.ProcessResponse(state: IInteractiveState, response: obj): IInteractiveState =      
            let state = (state:?>InteractiveState<RogueOneState>)
            state.ProcessResponse(response) 
                                    