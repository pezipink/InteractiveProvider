
//#r @"C:\repos\InteractiveProvider\ZMachine\ZMachine\bin\Debug\ZMachine.exe"
//
//open System
//open System.Text
//open System.IO
//let MachineAgent = MailboxProcessor<string * AsyncReplyChannel<string * Machine.Machine>>.Start( fun inbox -> 
//    let sb = StringBuilder()    
//    let rec loop (reply:AsyncReplyChannel<string * Machine.Machine>,machine) = async {
//        // get the next instruction and process it immediately if it is not SREAD 
//        let (zm, instruction) = Machine.getNextInstruction machine
//        match instruction with
//        | Machine.VarOp,    0x4, [text; parse] -> 
//            // this is a parse instruction, we will suspend execution here (before the decode happened) 
//            // until the user has built up a string to use. 
//            reply.Reply(sb.ToString(), zm)
//            sb.Clear() |> ignore
//            let! (input,reply) = inbox.Receive()
//            let zm = Instructions.processOpcode {zm  with Read = fun () -> input } instruction
//            return! loop (reply,zm)
//        | _ -> 
//            let zm = Instructions.processOpcode zm instruction
//            return! loop (reply,zm) }
//
//    let zm = Machine.createMachine (sb.Append >> ignore) Console.ReadLine  @"zork1.z5" 
//    let (input,reply) = inbox.Receive() |> Async.RunSynchronously
//    loop (reply,zm))   
//
//
//let x = MachineAgent.PostAndReply(fun reply -> "",reply)
//System.Console.WriteLine(fst x)
//
//let y = MachineAgent.PostAndReply(fun reply -> "open mailbox",reply)
//System.Console.WriteLine(fst y)
//
//let z = MachineAgent.PostAndReply(fun reply -> "read leaflet",reply)
//System.Console.WriteLine(fst z)
//
//

let chaos = System.Random()

let lookup = [|0..255|]

Array.sortInPlaceWith(fun _ _ -> chaos.Next(0,256).CompareTo(chaos.Next(0,256))) lookup


let phash (b1:int) (b2:int) =
    let h1 = lookup.[0 ^^^ b1]
    let h2 = lookup.[h1 ^^^ b2]
    let h3 = lookup.[h2 ^^^ h1]
    (h1 <<< 16) ||| (h2 <<< 8) ||| h3 |> uint32


phash 0 1


