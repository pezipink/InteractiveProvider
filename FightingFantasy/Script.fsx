open System
open System.Text

let data = IO.File.ReadAllLines("I:\\wolf.txt")

let results = 
    (([],(0,[])),data)
    ||> Array.fold( fun (finished,(number,current)) line -> 
        match Int32.TryParse line with
        | true, i  -> ((number,current)::finished,(i,[]))
        | _ -> (finished, (number, line :: current)))



//        INSERT INTO "main"."ReferenceText" ("reference_key","reference_text","game_id") VALUES ("%s","%s",%s)


let stmt =
    results
    |> fst
    |> List.map(fun (number, lines) -> 
        let line = String.Join("\r\n", lines)
        sprintf "INSERT INTO \"main\".\"ReferenceText\" (\"reference_key\",\"reference_text\",\"game_id\") VALUES (\"%i\",\"%s\",%i)" number line 1
        )

IO.File.WriteAllLines("I:\wolf.stmt", stmt)