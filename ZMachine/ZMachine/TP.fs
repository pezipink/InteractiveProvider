module ZMachineTP

open System
open System.Text
open InteractiveProvider
open InteractiveProvider.Interfaces

    
type ZMachineState =
     | Introduction
     | Game of Machine.Machine * string * string
     | PreProcess of Machine.Machine * string
     | Win        

type ZMachineMessage =
    | Processing of Machine.Machine * StringBuilder * AsyncReplyChannel<string * Machine.Machine>
    | ReadInput
    | UserInput of Machine.Machine * string * AsyncReplyChannel<string * Machine.Machine>

let MachineAgent = MailboxProcessor<ZMachineMessage>.Start( fun inbox -> 
    let rec loop message = async {
        // get the next instruction and process it immediately if it is not SREAD 
        match message with
        | ReadInput ->
            let! msg = inbox.Receive()
            // Processing could be called here from a new machine instance, if not 
            // then this must be actual user input as per the suspended call below.
            match msg with
            | Processing _ as x -> return! loop x
            | ReadInput _ as x -> return! loop x
            | UserInput (zm,input,reply) ->
                let (zm, instruction) = Machine.getNextInstruction zm
                // this must be the previous instruction to get text so just execute it
                let zm = Instructions.processOpcode {zm  with Read = fun () -> input.Trim() } instruction    
                return! loop (Processing(zm, new StringBuilder(), reply))

        | Processing (zm,sb,reply) ->
            let (zm2, instruction) = Machine.getNextInstruction zm
            match instruction with
            | Machine.VarOp,    0x4, [_; _] -> 
                // this is a parse instruction, we will suspend execution here
                // until the user has built up a string to use. 
                reply.Reply(sb.ToString(), zm)                
                return! loop ReadInput
            | _ -> 
                let zm = Instructions.processOpcode {zm2 with Write = sb.Append >> ignore} instruction
                return! loop (Processing(zm,sb,reply)) }

    loop ReadInput)  
let rec preprocess(Game (zm,output,input)) =
    { displayOptions = fun d -> 
        let d = Text.readDictionary zm.Memory  (uint32 zm.Header.DictLoc) zm.Header.AbbrevLoc      
        [
            ("#Contine", box "") ]
      displayText = fun _ -> "processing..."
      processResponse = fun (e,i) -> 
        let (output,zm) = MachineAgent.PostAndReply(fun reply -> UserInput(zm,input,reply))
        game(Game (zm,output,"")) :> _
      state = Game (zm,output,input) 
    }

and game(Game (zm,output,input)) =
    { displayOptions = fun d -> 
        let d = Text.readDictionary zm.Memory  (uint32 zm.Header.DictLoc) zm.Header.AbbrevLoc      
        [
            ("#Current Output", box "#Current Output")
            ("#Enter", box "#Enter") ]
        @
        [for kvp in d -> 
            kvp.Key, box kvp.Key]
      displayText = fun _ -> output  + "\r\n" + "Current Input : " + input |> wrapAndSplit
      processResponse = fun (e,i) -> 
        match (i:?>string) with
        | "#Current Output" -> game (Game (zm,output,input)) :> _
        | "#Enter" -> preprocess (Game (zm,output,input)) :> _
        | value -> game (Game (zm,output,input + " " + value)) :> _
      state = Game (zm,output,input) 
    }


type ZMachine() =
    interface IInteractiveServer with
        member x.NewState: IInteractiveState =
            let sb = StringBuilder()
            let zm = Machine.createMachine (sb.Append >> ignore) (fun () -> "") @"C:\repos\ZMachine\ZMachine\ZMachine\zork1.z5"
            let y = MachineAgent.PostAndReply(fun reply -> Processing(zm,sb,reply))
            game (Game(snd y,fst y,"")) :> IInteractiveState        
        member x.ProcessResponse(state: IInteractiveState, response: obj): IInteractiveState =      
            let state = (state:?>InteractiveState<ZMachineState>)
            state.ProcessResponse(response) 
                                    