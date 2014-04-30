//namespace InteractiveProvider.Interfaces
//
//type IInteractiveClient =
//    abstract member DisplayText : string
//    abstract member DisplayOptions : (string * int) list
//
//type IInteractiveServer = 
//    abstract member NewState : IInteractiveClient
//    abstract member ProcessResponse : IInteractiveClient * int -> IInteractiveClient

namespace InteractiveProvider

open InteractiveProvider.Interfaces
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type InteractiveProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    do         
        
        System.AppDomain.CurrentDomain.add_AssemblyResolve( System.ResolveEventHandler(fun _ e -> 
            let fi = System.IO.FileInfo(config.RuntimeAssembly)
            Assembly.LoadFile(System.IO.Path.Combine(fi.DirectoryName, ( e.Name.Substring(0,e.Name.IndexOf(",")) + ".dll" )))))

    let ns = "PinkSquirrels.Interactive"
    let asm = Assembly.GetExecutingAssembly()
    
    
//    do coreType.AddXmlDoc("<summary><para>Welcome to the pinksquirrellabs.com interactive type provider!</para>
//                                    <para>With this type provider you can play many text-based games, from classic BASIC games</para>
//                                    <para>through to MineSweeper and Fighting Fantasy!</para>
//                                    <para></para>
//                                    <para>You can also easily write your own games, see github and please send a pull request!</para></summary>")
//    
    
    let createTypes (location:string) (rootTypeName:string) =
        let newName() = System.Guid.NewGuid().ToString()
        let rootType = ProvidedTypeDefinition(asm,ns,rootTypeName, None, HideObjectMethods = true)    
        rootType.AddMember(ProvidedConstructor([], InvokeCode = fun _ -> <@@ obj() @@>))

        let rec createStages(gameType:ProvidedTypeDefinition, name:string, server:IInteractiveServer, client:IInteractiveState) =
            let ty = ProvidedTypeDefinition(name,None)
            gameType.AddMember ty
            ty.AddMembersDelayed(fun _ -> 
                client.DisplayOptions
                |> List.map(fun (text,index) -> 
                    let next = server.ProcessResponse(client,index)
                    let p = ProvidedProperty(text,createStages(gameType,(newName()), server, next), GetterCode = fun args -> <@@ obj()  @@>)
                    p.AddXmlDoc next.DisplayText
                    p ))
            ty
        
        let getGameServers() =
            System.IO.Directory.GetFiles(location,"*.dll")
            |> Array.filter(fun file -> file <> "InteractiveProvider.dll" && file <> "InteractiveProviderInterfaces.dll"  && file <> "FSharp.Core.dll" )
            |> Array.collect(fun file ->
                try
                    let asm = Assembly.LoadFile(file)
                    asm.GetTypes()
                    |> Array.choose(fun t -> 
                        t.GetInterfaces() 
                        |> Array.exists(fun i -> i.Name = "IInteractiveServer")
                        |> fun b -> if b then Some t else None)
                with
                | _ -> [||] )

        getGameServers()
        |> Array.map(fun t -> 
            let gt = ProvidedTypeDefinition(t.Name,None)
            let server = System.Activator.CreateInstance(t) 
            let server' = server :?> IInteractiveServer
            let ty = createStages(gt, (newName()), server', server'.NewState)
            let p = ProvidedProperty("Start " + t.Name, ty, GetterCode = fun args -> <@@ obj() @@>)
            p.AddXmlDoc(server'.NewState.DisplayText)
            rootType.AddMemberDelayed(fun _ -> p)
            gt)
        |> Array.iter( fun t -> rootType.AddMember t)

        rootType
        
    
    let paramType = ProvidedTypeDefinition(asm,ns,"InteractiveProvider", None, HideObjectMethods = true) 
    let loadLocation = ProvidedStaticParameter("GameLocation", typeof<string>)
    do paramType.DefineStaticParameters([loadLocation], fun typeName args -> createTypes (args.[0]:?>string) typeName)
    do this.AddNamespace(ns, [paramType])                

[<assembly:TypeProviderAssembly>]
do ()
    