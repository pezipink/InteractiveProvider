module Text

open Memory
    
type PrintMode =
    | Alphabet0
    | Alphabet1
    | Alphabet2
    | Abbreviation of uint16
    
let toZChars startAddress readWord =
    let rec aux currentAddress currentChars=
        let word = readWord currentAddress 
        let finished = word &&&= 0x8000us 
        let c1 = (word >>> 10) &&& 0x1Fus
        let c2 = (word >>> 5) &&& 0x1Fus
        let c3 = word &&& 0x1Fus
        if finished then 
            currentAddress, List.rev (c3 :: c2 :: c1 :: currentChars)
        else aux (currentAddress+2u) (c3 :: c2 :: c1 :: currentChars)
    aux startAddress []

let getZscii = function
    | 0us -> char 0 // null
    | 8us -> char 0 // delete
    | 13us -> '\n'  // newline
    | 27us -> char 0
    | n when n >= 129us && n <= 154us -> char 0
    | n when (n >= 32us && n <= 126us) || (n>=155us && n<= 251us) -> char n
    | _ -> failwith "invalid 10bit zscii char"

let printZChars chars memory abbrevLoc write =
    let a0 = "abcdefghijklmnopqrstuvwxyz"
    let a1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let a2 = """ ^0123456789.,!?_#'"/\-:()" """

    let rec aux mode chars =
        match mode, (chars:uint16 list) with
        | _, [] -> ()
        | Abbreviation n, c :: t ->
            let index = uint16 <| (n-1us) * 32us + c
            let address =
                memory.ReadWord(
                    uint32<| abbrevLoc+index*2us)
            toZChars (uint32(address*2us)) memory.ReadWord
            |> snd
            |> aux Alphabet0
            aux Alphabet0 t           
        | _, 0us :: t ->
            // special case space character
            write <| sprintf " "
            aux Alphabet0 t
        | _, 4us :: t ->
            // switch to alphabet 1
            aux Alphabet1 t
        | _, 5us :: t ->
            // switch to alphabet 2
            aux Alphabet2 t
        | _, n :: t when n = 1us || n = 2us || n = 3us ->
            // abbreviation table 1, 2 or 3
            aux (Abbreviation n) t
        | Alphabet0, c :: t ->     
            // write from alphabet 0           
            write <| sprintf "%c" (a0.[(int c)-6])
            aux Alphabet0 t
        | Alphabet1, c :: t ->
            // write from alphabet 1
            write <| sprintf "%c" (a1.[(int c)-6])
            aux Alphabet0 t 
        | Alphabet2, 6us :: a :: b :: t ->
            // special case zscii encoding 
            // the next 2 z characters specify a 10 bit zscii code                
            write <| (getZscii ((a <<< 5) ||| b)).ToString()
            aux Alphabet0 t
        | Alphabet2, 7us :: t ->
            // special case newline
            write <| "\n"
            aux Alphabet0 t
        | Alphabet2, c :: t ->
            // write from alphabet 2
            write <| sprintf "%c" (a2.[(int c)-6])
            aux Alphabet0 t
        | _, _ :: t ->
            // ??
            write <| sprintf "<uknown>"
            aux (Alphabet0) t

    aux Alphabet0 (Seq.toList chars)


let readDictionary mem dictLoc abbrevLoc =
    // header consists of a byte indicating word seperator
    // chars, follwed by the zchars thenseles, then a byte
    // for word lengths, a word for the word count and finally
    // the words themselves
    let numSeps = uint32 <| mem.ReadByte dictLoc
    let sepsLoc = dictLoc+1u
    let seps = [for x in sepsLoc .. sepsLoc + numSeps-1u ->
                mem.ReadByte x]
    let wordLen =   uint32 <| mem.ReadByte (dictLoc + numSeps + 1u)
    let wordCount = uint32 <| mem.ReadWord (dictLoc + numSeps + 2u)
    let entryStart = dictLoc + numSeps + 4u
    let entries = [for x in entryStart .. wordLen .. entryStart + (wordLen * wordCount) ->
                    x,[for i in 0u .. (wordLen - 1u) -> mem.ReadByte (x + i)]]
    let sb = new System.Text.StringBuilder()
    entries
    |> List.map(fun (address, _) -> 
        sb.Clear() |> ignore
        let x, y = toZChars address mem.ReadWord
        printZChars y mem abbrevLoc (sb.Append >>ignore)
        sb.ToString(),address)
    |> Map.ofList

let asciiEncode (input:string) memory address =
    ((memory,address), input) 
    ||> Seq.fold(fun (m,c) v -> (m.WriteByte c (byte v),(c+1u)))
    |> fst

let readInput mem dictLoc abbrevLoc textBuffer parseBuffer (inputText:string) =
    let maxText = uint16 ((mem.ReadByte textBuffer) - 1uy)
    let maxParse = uint16 (mem.ReadByte parseBuffer) 
    // read and clean up the text
    let text = inputText.TrimEnd('\n').ToLower() 
    
    // write as ascii into the text buffer
    let mem = asciiEncode (text+ (string '\000')) mem (textBuffer+1u)    
    if maxParse = 0us then mem else
    // parse the string remembering word start indexes
    let rec parse currentWord index words = function
        | [] -> if currentWord <> "" then 
                    (index+1-currentWord.Length, currentWord) :: words
                else words
        | c :: t when c = ' ' -> 
            if currentWord = "" then  parse currentWord (index+1) words t
            else parse "" (index+1) ((index+1-currentWord.Length,currentWord) :: words) t
        | c :: t -> 
            parse (currentWord+(string c)) (index+1) words t
    let parseTable = parse "" 0 [] (Seq.toList text) |> Map.ofList
    
    // not sure if the dict can change during the exeuction and thus safe to cache ...
    let dict = readDictionary mem dictLoc abbrevLoc
    // find matching words form the dictionary and store 3 bytes in the parse buffer :
    // dict location (or 0), number of letters in word, start index from text buffer.
    let matches = 
        parseTable
        |> Map.map(fun location word -> 
            let choppedWord = if word.Length > 6 then word.Remove 6 else word
            match dict.TryFind choppedWord with
            | Some v -> v, word.Length, location
            | None -> 0u, word.Length, location)
    
    // amount of words go in byte 1
    let mem = mem.WriteByte (uint32(parseBuffer+1u)) (byte parseTable.Count)
    
    // write the map out as above
    let mem =
        ((mem,0u),matches)
        ||> Map.fold(fun (mem, index) _ (dictIndex, size, location) -> 
            let address = (uint32 parseBuffer) + 2u + (index * 4u)
            let mem = mem.WriteWord address (uint16 dictIndex)
            let mem = mem.WriteByte (address+2u) (byte size)
            let mem = mem.WriteByte (address+3u) (byte location)
            let mem = mem.WriteByte (address+4u) 0uy
            mem, (index+1u))
        |> fst
      
    mem

