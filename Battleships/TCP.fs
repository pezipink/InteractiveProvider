module TCP


open System.Net
open System.Net.Sockets
open System.Collections.Generic
open System.Runtime.Serialization
open Microsoft.FSharp.Core.CompilerServices

type MessagePair<'output, 'input> = { message : 'output; mutable result : 'input  }

let private toIList<'T> (data : 'T array) =
    let segment = new System.ArraySegment<'T>(data)
    let data = new List<System.ArraySegment<'T>>() :> IList<System.ArraySegment<'T>>
    data.Add(segment)
    data

type Socket with 
    member this.AcceptAsync() =
        Async.FromBeginEnd((fun (callback, state) -> this.BeginAccept(callback, state)),
                            this.EndAccept)
    member this.ConnectAsync(ipAddress : IPAddress, port : int) =
        Async.FromBeginEnd(ipAddress, port,
                            (fun (ipAddress:IPAddress, port, callback, state) ->
                                this.BeginConnect(ipAddress, port, callback, state)),
                            this.EndConnect)
    member this.SendAsync(data : byte array, flags : SocketFlags) =
        Async.FromBeginEnd(toIList data, flags, 
                            (fun (data : IList<System.ArraySegment<byte>>,
                                    flags : SocketFlags, callback, state) ->
                                        this.BeginSend(data, flags, callback, state)),
                            this.EndSend)
    member this.ReceiveAsync(data : byte array, flags : SocketFlags) =
        Async.FromBeginEnd(toIList data, flags, 
                            (fun (data : IList<System.ArraySegment<byte>>,
                                    flags : SocketFlags, callback, state) ->
                                        this.BeginReceive(data, flags, callback, state)),
                            this.EndReceive)


let private ipHostInfo = Dns.Resolve(Dns.GetHostName())
let private port = 11000

type SocketType = Server | Client of string

let createSocket socketType = 
    let socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    match socketType with
    | Server ->
        let localIPAddress = ipHostInfo.AddressList.[0]    
        let localEndPoint = new IPEndPoint(localIPAddress, port)
        socket.Bind(localEndPoint)
    | Client ipString ->
        let ipAddress = IPAddress.Parse(ipString)
        let remoteEndPoint  = new IPEndPoint(ipAddress, port)
        socket.Bind(remoteEndPoint)
    socket

//
//let connectSendReceive (socket : Socket) =
//    async {
//        do! socket.ConnectAsync(ipHostInfo.AddressList.[0], 11000)
//        let buffer1 = [| 0uy .. 255uy |]
//        let buffer2 = Array.zeroCreate<byte> 255
//        let flags = new SocketFlags()
//        let! flag = socket.SendAsync(buffer1, flags)
//        let! result = socket.ReceiveAsync(buffer2, flags)
//        return buffer2
//    }

let connectReceiveSend (socket : Socket)  (messagePair : MessagePair<'a, 'b>) =
    async {
        do! socket.ConnectAsync(ipHostInfo.AddressList.[0], 11000)
        let flags = new SocketFlags()
        let fmt = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
 
        let inBuffer = Array.zeroCreate<byte> 255        
        printfn "Receiving data..." 
        let! result = socket.ReceiveAsync(inBuffer, flags)
        
        use ms = new System.IO.MemoryStream(inBuffer)
        ms.Position <- 0L
        messagePair.result <- fmt.Deserialize(ms) :?> _

        let outBuffer =
            use ms = new System.IO.MemoryStream()
            fmt.Serialize(ms, messagePair.message)
            ms.ToArray()
        printfn "Sending data..." 
        let! flag = socket.SendAsync(outBuffer, flags)        


        return messagePair }


let connectSendReceive (socket : Socket)  (messagePair : MessagePair<'a, 'b>) =
    async {
        do! socket.ConnectAsync(ipHostInfo.AddressList.[0], 11000)
        let flags = new SocketFlags()
        printfn "Connected to remote host." 
        let fmt = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        let outBuffer =
            use ms = new System.IO.MemoryStream()
            fmt.Serialize(ms, messagePair.message)
            ms.ToArray()
        printfn "Sending data..." 
        let! flag = socket.SendAsync(outBuffer, flags)

        let inBuffer = Array.zeroCreate<byte> 255        
        printfn "Receiving data..." 
        let! result = socket.ReceiveAsync(inBuffer, flags)
        
        use ms = new System.IO.MemoryStream(inBuffer)
        ms.Position <- 0L
        messagePair.result <- fmt.Deserialize(ms) :?> _
        return messagePair }


//let acceptReceiveSend (socket : Socket) (messagePair : MessagePair<'a, 'b>) =
//    async {
//        socket.Listen(10)
//        let! socket = socket.AcceptAsync()
//        let buffer1 = Array.zeroCreate<byte> 256
//        let flags = new SocketFlags()
//        let! nBytes = socket.ReceiveAsync(buffer1, flags)
//        let buffer2 = Array.rev buffer1
//        let f = System.Runtime.Serialization.NetDataContractSerializer()
//        
//        use ms = new System.IO.MemoryStream(buffer2)
//        ms.Position <- 0L
//        use reader = System.Xml.XmlReader.Create(ms)
//        let f = System.Runtime.Serialization.NetDataContractSerializer()
//        let x = f.ReadObject(reader) :?> Microsoft.FSharp.Quotations.Expr<Async<int>>
//
//        
//        
//        use ms = new System.IO.MemoryStream()
//        use writer = System.Xml.XmlWriter.Create(ms)
//        let e =    <@ 
//            async {
//                let! _ = async { return 1 }
//                return 1
//            } 
//        @>
//
//        //f.WriteObject(writer, <@ ProvidedTypeDefinition("JOHN",Some typeof<string>) @> )
//        f.WriteObject(writer, e )
//        writer.Flush()
//        ms.Position <- 0L        
//        let arrbyte = ms.ToArray();
//        printfn "Sending..." 
//        let! flag = socket.SendAsync(arrbyte, flags)
//        //ms.Close()
//        printfn "Completed." 
//        return buffer2
//    }
//
//let taskServer = Async.StartAsTask(acceptReceiveSend(socket))    
//taskServer.Wait()
//socket.Close()
//
