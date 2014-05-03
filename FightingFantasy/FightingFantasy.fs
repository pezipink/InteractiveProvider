namespace FightingFantasy

type Dice() =
    let rng = System.Random(System.DateTime.Now.Millisecond)
    let d6() = rng.Next(1,7)

type Skill = int
type Luck = int
type Stamina = int

type Player =
    { skill : Skill * Skill
      Luck : Luck * Luck
      Stamina : Stamina * Stamina }

type GlobalState =
    { player : Player
      dice : Dice
    }


type Class1() = 
    member this.X = "F#"
