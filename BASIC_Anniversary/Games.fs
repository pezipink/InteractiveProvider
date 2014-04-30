namespace BASIC_Anniversary

open InteractiveProvider.Interfaces
open System

module Utils =
   let rnd =  System.Random(System.DateTime.Now.Millisecond)
   let wrapAndSplit (text:string) =
    let text = 
        text.Split([|Environment.NewLine|], StringSplitOptions.None)
        |> Array.map(fun s -> String.Format("<para>{0}</para>", s.Replace(" "," ")))
        |> fun t ->  String.Join("\r\n", t)
    String.Format("<summary>{0}</summary>", text)

type FlipFlopState =
    { Xs : int array; start : bool; rnd : float; prev : int; count : int }
    with
        interface IInteractiveState with
            member this.DisplayText = 
                if this.start then
                    """                     FLIP FLOP
                    CREATIVE COMPUTING MORRISTOWN, NEW JERSEY
                    THE OBJECT OF THE PUZZLE IS TO CHANGE THIS :
            
                    X X X X X X X X X X

                    TO THIS :
                     
                    0 0 0 0 0 0 0 0 0 0

                    BY TYPING THE NUMBER CORRESPONDING TO THE POSITION OF THE
                    LETTER ON SOME NUMBERS, ONE POSITION WILL CHANGE, ON
                    OTHERS, TWO WILL CHANGE.  TP RESET LINE TO ALL X'S, TYPE 0
                    (ZERO) AND TO START OVER IN THE MIDDLE OF A GAME, TYPE 
                    11 (ELEVEN)
            
                    HERE IS THE STARTING LINE OF X'S.
            
                    1 2 3 4 5 6 7 8 9 10
                    X X X X X X X X X X 
            
                    INPUT THE NUMBER?""" |> Utils.wrapAndSplit
                else
                    String.Join(" ", [for i in 1..10 -> i.ToString()]) 
                    + Environment.NewLine 
                    + String.Join(" ", [for x in this.Xs -> if x = 0 then "0" else "X"])
                    |> Utils.wrapAndSplit 
            member __.DisplayOptions = [0..11] |> List.map(fun i -> (i.ToString(),i)) 

type FlipFlop() =
    interface IInteractiveServer with
        member this.NewState = 
            { FlipFlopState.Xs = [|for x in 1..10 -> 1|]; 
              start = true; rnd = Utils.rnd.NextDouble(); prev = 0; count = 0 } :> IInteractiveState
        member this.ProcessResponse(client,choice) =
            let state = client :?> FlipFlopState
            let data = Array.copy state.Xs
            match choice with
            | 0 -> { state with FlipFlopState.Xs = [|for x in 1..10 -> 1|] } :> IInteractiveState 
            | 11 -> (this:>IInteractiveServer).NewState
            | x -> 
                if data.[x-1] = 1 then data.[x-1] <- 0 else data.[x-1] <- 1
                { state with FlipFlopState.Xs = data } :> IInteractiveState 
            

//            let auxb rev curr

//            match choice with
//            | 11 -> (this:>IInteractiveServer).NewState
//            | 0 -> { state with FlipFlopState.Xs = [|for x in 1..10 -> 1|] } :> IInteractiveClient
//            | curr when curr = state.prev ->
//                if data[curr] = 0 then // 510
//                    let rec aux curr =
//                        data.[curr] <- 1  // 590
//                        let r = 0.592 * (1.0/tan(state.rnd/(float curr)+state.rnd)/sin(curr*2.0+state.rnd)-cos(float curr)) // 530
//                        let curr' = r - int(r)      // 540
//                        let curr = int(10.0*curr')  // 550
//                        if data.[curr] = 0 then aux curr
//                        else 
//                            data.[curr] <- 0
//                            { state with FlipFlopState.Xs = data; prev = curr; count = state.count + 1}
//                    aux curr

                    
//            | curr ->
                
//            
//            data.[choice-1] <- 0
//            {state with Xs = data; start = false } :> IInteractiveClient


