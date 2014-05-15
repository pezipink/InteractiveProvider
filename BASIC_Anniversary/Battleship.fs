module Battleship
open InteractiveProvider.Interfaces
open System
open System.Text

type Square =
    | Empty
    | Hit
    | Target
    override this.ToString() =
        match this with Empty | Target -> " " | Hit -> "#"
type BattleshipState =
    { squares: Square array array }
    interface IInteractiveState with
        member this.DisplayText =
            let sb = StringBuilder()
            let append (s:string) = sb.Append s |> ignore
            append "<summary>"
            this.squares
            |> Array.iter (fun s ->
                s |> Array.iter (fun i ->
                    append "<para>"
                    sb.AppendFormat("|{0}", i) |> ignore
                    append "|</para>"))
            append "</summary>"
            sb.ToString()
        member this.DisplayOptions =
            let options =
                [for i in 0..10 -> [for j in 0..10 -> i,j]]
                |> List.collect id
                |> List.map (fun (x,y) -> sprintf "Ensign, target X%i Y%i !" x y, box (x,y))
            ("# Battleship Grid", box (-1,-1)) :: options

type Battleship() =
    let rand = Random().Next
    let createGrid width height =
        let addShip size grid =
            grid
        addShip 0 0
    interface IInteractiveServer with
        member this.NewState =
            { squares=Array.empty } :> IInteractiveState
        member this.ProcessResponse(state,choice) =
            { squares=Array.empty } :> IInteractiveState
