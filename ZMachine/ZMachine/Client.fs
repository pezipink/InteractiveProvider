module TestClient
open System
open System.Text
open System.IO
open Machine
open Instructions

let p = sprintf
let d = System.Diagnostics.Debug.WriteLine


// fsi.AddPrinter<seq<byte>>(
//     fun t -> "[" + String.Join("; ", t |> Seq.map(fun v -> String.Format("{0:X2}", v))) + "]")
// fsi.AddPrinter<seq<int>>(
//     fun t -> "[" + String.Join("; ", t |> Seq.map(fun v -> String.Format("{0:X2}", v))) + "]")

//toZChars(header.AbbrevLoc) |> snd |> printZChars
// let dictionary =
//     // header consists of a byte indicating word seperator
//     // chars, follwed by the zchars thenseles, then a byte
//     // for word lengths, a word for the word count and finally
//     // the words themselves
//     let numSeps = int <| allBytes.[header.DictLoc] 
//     let sepsLoc = header.DictLoc+1
//     let seps = [for x in sepsLoc .. sepsLoc + (numSeps-1) ->
//                 allBytes.[x]]
//     let wordLen = int <| allBytes.[header.DictLoc + numSeps + 1]
//     let wordCount = int <| readWord (header.DictLoc + numSeps + 2) allBytes
//     let entryStart = header.DictLoc + numSeps + 4
//     let entries = [for x in entryStart .. wordLen .. entryStart + (wordLen * wordCount) ->
//                    [for i in 0 .. (wordLen - 1) -> allBytes.[x + i]]]
//     entries
    

// let objectTable =
//     let defaults = [for x in 0 .. 30 -> readWord (header.ObjLoc+(x*2)) allBytes]
//     let objectStart = header.ObjLoc + 31 * 2
//     let readObject n =
//         [for x in 0 .. 8 -> readByte(n+x) allBytes]
      
//     for x in 0..219 do
//         let first = readObject (objectStart+(x*9))
//         let properties = ((int first.[7]) <<< 8) ||| (int first.[8])
//         let textLength = int <| readByte properties allBytes
//         let textStart = properties + 1
//         let name = [for x in 0..textLength -> readWord (textStart+(x*2)) allBytes]
//         if textLength = 0 then printfn "" else
//         toZChars textStart |> snd |> printZChars 
//     ()
//let zork =
//    let markers = Map.empty : Map<string, Machine>
//    Machine.createMachine Console.Write Console.ReadLine  @"zork1.z5" 
//    |> Seq.unfold( fun machine ->  
//    //    p ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" |> d
//        let next = getNextInstruction machine ||>processOpcode
//        Some (next, next))

//let x = createObjectManager machine.Header.ObjLoc machine.Header.AbbrevLoc
//    
//let y = x.GetObject machine.Memory 156us
//let a = x.GetObjectProperty machine.Memory 156us 17us
//let b = x.SetObjectProperty machine.Memory 156us 17us 42us
//
//let c = x.GetObjectProperty b 156us 17us

//Seq.iter(fun _ -> ()) zork
//System.Console.ReadLine()
//
//
//
 
//MachineAgent.PostAndReply(fun reply -> UserInput ("" ,reply))







open System
open System.Text
open System.IO

let MachineAgent = MailboxProcessor<string * AsyncReplyChannel<string * Machine.Machine>>.Start( fun inbox -> 
    let sb = StringBuilder()    
    let rec loop (reply:AsyncReplyChannel<string * Machine.Machine>,machine) = async {
        // get the next instruction and process it immediately if it is not SREAD 
        let (zm, instruction) = Machine.getNextInstruction machine
        match instruction with
        | Machine.VarOp,    0x4, [_; _] -> 
            // this is a parse instruction, we will suspend execution here
            // until the user has built up a string to use. 
            reply.Reply(sb.ToString(), zm)
            sb.Clear() |> ignore
            let! (input,reply) = inbox.Receive()
            let zm = Instructions.processOpcode {zm  with Read = fun () -> input } instruction
            return! loop (reply,zm)
        | _ -> 
            let zm = Instructions.processOpcode zm instruction
            return! loop (reply,zm) }

    let zm = Machine.createMachine (sb.Append >> ignore) Console.ReadLine  @"C:\repos\ZMachine\ZMachine\ZMachine\zork1.z5" 
    let (input,reply) = inbox.Receive() |> Async.RunSynchronously
    loop (reply,zm))   


let x = MachineAgent.PostAndReply(fun reply -> "",reply)
System.Console.WriteLine(fst x)
while true do
    let input = System.Console.ReadLine()    
    let y = MachineAgent.PostAndReply(fun reply -> input,reply)
    System.Console.WriteLine(fst y)
