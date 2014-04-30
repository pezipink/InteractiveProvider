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
            member __.DisplayOptions = [0..11] |> List.map(fun i -> (i.ToString(),box i)) 

type FlipFlop() =
    interface IInteractiveServer with
        member this.NewState = 
            { FlipFlopState.Xs = [|for x in 1..10 -> 1|]; 
              start = true; rnd = Utils.rnd.NextDouble(); prev = 0; count = 0 } :> IInteractiveState
        member this.ProcessResponse(client,choice) =
            let state = client :?> FlipFlopState
            let data = Array.copy state.Xs
            let choice = unbox<int> choice
            match choice with
            | 0 -> { state with FlipFlopState.Xs = [|for x in 1..10 -> 1|] } :> IInteractiveState 
            | 11 -> (this:>IInteractiveServer).NewState
            | x -> 
                if data.[x-1] = 1 then data.[x-1] <- 0 else data.[x-1] <- 1
                { state with FlipFlopState.Xs = data; start = false } :> IInteractiveState 
            

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


type RockPaperScissorsSuccessStates =
    | Win
    | Lose 
    | Draw

type RockPaperScissorsStates =
    | Rock
    | Paper
    | Scissors
    with 
        member this.winner(otherState) =
            match this, otherState with
            | Rock, Paper -> Lose
            | Rock, Rock ->  Draw
            | Rock, Scissors -> Win
            | Paper, Rock -> Win
            | Paper, Paper -> Draw
            | Paper, Scissors -> Lose
            | Scissors, Paper -> Win
            | Scissors, Rock -> Lose
            | Scissors, Scissors -> Draw
        override this.ToString() =
            match this with
            | Rock -> "ROCK"
            | Paper -> "PAPER"
            | Scissors -> "SCISSORS"

type RockPaperScissorsState = 
    { maxGames:int; currentGame:int; wins: int; losses: int; draws: int; 
      lastChoice : RockPaperScissorsStates; lastAIChoice : RockPaperScissorsStates; showResult:bool}
    with    
    member this.GetText() =
        let result = 
         " YOUR CHOICE WAS " + this.lastChoice.ToString() + Environment.NewLine +
                " THIS IS MY CHOICE... 
                ... " + this.lastAIChoice.ToString() + Environment.NewLine +
                    match this.lastChoice.winner(this.lastAIChoice) with
                    | Win -> " YOU WIN!!!"
                    | Lose -> " WOW! I WIN!!"
                    | Draw -> " TIE GAME. NO WINNER."  + Environment.NewLine 
        match this with
           | { maxGames = 0; currentGame = 0} ->
              """  GAME OF ROCK, SCISSORS AND PAPER
                   CREATIVE COMPUTING MORRISTOWN, NEW JERSEY
                    
                   HOW MANY GAMES WOULD YOU LIKE TO PLAY?""" |> Utils.wrapAndSplit
           | state when state.maxGames+1 = state.currentGame -> 
               result + Environment.NewLine +
               "GAME OVER! " + Environment.NewLine + Environment.NewLine +
               "HERE IS THE FINAL GAME SCORE " + Environment.NewLine +
               (sprintf "I HAVE WON %i GAME(S).\r\nYOU HAVE WON %i GAME(S).\r\nAND %i GAMES ENDED IN A TIE." state.losses state.wins state.draws)
               |> Utils.wrapAndSplit
           | {currentGame = 1} as state ->
               "GAME NUMBER " + state.currentGame.ToString() + Environment.NewLine +
               "WHAT'S YOUR CHOICE?" 
               |> Utils.wrapAndSplit
           | state ->                 
               result 
               + "GAME NUMBER " + state.currentGame.ToString() + Environment.NewLine +
               "WHAT'S YOUR CHOICE?" 
               |> Utils.wrapAndSplit
                
    interface IInteractiveState with
          member this.DisplayText = 
            if this.showResult then this.GetText()
            else  "SECRET!" // don't show anything on the property until they press dot (and then show it on the # RESULT property), else that would be cheating!
           
          member this.DisplayOptions =
            match this with
            | { maxGames = 0; currentGame = 0} -> [1..10] |> List.map(fun i -> (i.ToString(),box i)) 
            | state when state.maxGames+1 = state.currentGame -> ["# RESULT", box None]
            | state -> ["# RESULT", box None; "ROCK", box (Some Rock); "PAPER", box (Some Paper); "SCISSORS", box (Some Scissors )]

type RockPaperScissors() =
    interface IInteractiveServer  with
        member this.NewState = { maxGames = 0; currentGame = 0; wins = 0; draws = 0; 
                                 losses = 0; lastChoice=Paper; lastAIChoice=Paper; showResult=true } :> IInteractiveState
        member this.ProcessResponse(state,choice) =  
            let state = state :?> RockPaperScissorsState
            if state.currentGame = 0 && state.maxGames = 0 then 
                {state with maxGames = unbox<int>choice; currentGame = 1} :> IInteractiveState
            else
                match unbox<RockPaperScissorsStates option>choice with
                | None -> // this is the special state that shows the last result
                        {state with showResult = true} :> IInteractiveState
                | Some(move) ->
                    // get random AI move and return new state with updated bits
                    let aiMove = match Utils.rnd.Next(1,4) with
                                 | 1 -> Rock
                                 | 2 -> Paper
                                 | 3 -> Scissors
                    let result = move.winner(aiMove)
                    { state with currentGame = state.currentGame + 1; 
                                 wins = if result = Win then state.wins + 1 else state.wins
                                 losses = if result = Lose then state.losses + 1 else state.losses
                                 draws = if result = Draw then state.draws + 1 else state.draws
                                 lastAIChoice = aiMove
                                 lastChoice = move
                                 showResult = false }  :> IInteractiveState
            
