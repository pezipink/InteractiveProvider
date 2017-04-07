module Instructions

open Memory
open Machine
open Text

let mutable i_count = 0

let processOpcode (machine:Machine) (count, number, values) =
    System.Diagnostics.Debug.Write (sprintf"%i: %i - %A opcode %A : %A" machine.Pc i_count count number values)
    //if i_count = 980 then System.Diagnostics.Debugger.Break()
    i_count <- i_count + 1    
    match count, number, values with
    
    // ZERO OP
    | ZeroOp, 0x0, [] ->           //  RTRUE
        p "RTRUE" |> d
        machine.PopCall 1us
    
    | ZeroOp, 0x1, [] ->           //  RFALSE
        p "RFALSE" |> d
        machine.PopCall 0us
    
    | ZeroOp, 0x2, [] ->          // PRINT
        p "PRINT" |> d
        let newPc, zChars = toZChars machine.Pc machine.Memory.ReadWord
        printZChars zChars machine.Memory machine.Header.AbbrevLoc machine.Write
        {machine with Pc = newPc+2u}

    | ZeroOp, 0x3, [] ->          // PRINT
        p "PRINT" |> d
        let newPc, zChars = toZChars machine.Pc machine.Memory.ReadWord
        printZChars zChars machine.Memory machine.Header.AbbrevLoc machine.Write
        let m = { machine with Pc = newPc+2u}
        m.PopCall 1us
  
    | ZeroOp, 0x8, [] ->          // RET_POPPED
        p "RET_POPPED" |> d                
        let v,m = machine.PopStack()
        m.PopCall v
 
    | ZeroOp, 0x9, [] ->          // POP
        machine.GetVariable 0uy |> snd

    | ZeroOp, 0xB, [] ->          // NEW_LINE
        p"NEW_LINE" |> d
        machine.Write "\n"
        machine

    // ONE OP
    | OneOp,    0x0, [x] ->       // JZ
        p"JZ" |> d
        machine.Branch((x=0us))   

    | OneOp,    0x1, [obj] ->       // GET_SIBLING 
        p"GET_SIBLING" |> d
        let v = uint16 <| machine.Objects.GetObjectSibling machine.Memory (byte obj)
        let m = machine.SetReturnValue v
        m.Branch(v<>0us)
        
    
    | OneOp,    0x2, [obj] ->       // GET_CHILD 
        p"GET_CHILD" |> d
        let v = uint16 <| machine.Objects.GetObjectChild machine.Memory (byte obj)
        let m = machine.SetReturnValue v
        m.Branch(v<>0us)
            
    | OneOp,    0x3, [obj] ->       // GET_PARENT
        p"GET_PARENT" |> d
        machine.SetReturnValue(uint16<|machine.Objects.GetObjectParent machine.Memory (byte obj)) 
    
    | OneOp,    0x4, [propAddr] ->  // GET_PROP_LEN
       p"GET_PROP_LEN" |> d
       machine.SetReturnValue(machine.Objects.GetPropertyLength machine.Memory propAddr)

    | OneOp,    0x5, [var] ->     // INC
        p "INC" |> d
        // signed
        let v, m = machine.GetVariable (byte var)
        let newValue =  (uint16 ((int16 v) + 1s))
        m.SetVariable (byte var) newValue

    | OneOp,    0x6, [var] ->     // DEC
        p "DEC" |> d
        // signed
        let v, m = machine.GetVariable (byte var)
        let newValue =  (uint16 ((int16 v) - 1s))
        m.SetVariable (byte var) newValue

    | OneOp, 0x7, [addr] ->          // PRINT_ADDR
        p "PRINT_ADDR" |> d        
        let _, zChars = toZChars (uint32 addr) machine.Memory.ReadWord
        printZChars zChars machine.Memory machine.Header.AbbrevLoc machine.Write
        machine

    | OneOp, 0x9, [objId] ->          // PRINT_ADDR
        p "REMOVE_OBJ" |> d        
        { machine with Memory = machine.Objects.RemoveObject machine.Memory objId }
        
   
    | OneOp,    0xA, [obj] ->       // PRINT_OBJ
        p "PRINT_OBJ" |> d        
        let obj = machine.Objects.GetObject machine.Memory obj
        machine.Write obj.Name
        machine
    
    | OneOp,    0xB, [value] ->       // RET
        p "RET" |> d        
        machine.PopCall value

    | OneOp,    0xC, [offset] ->    // JUMP
        p "JUMP" |> d
        // this is not a normal branch instruction, just modify the Pc by (sgined) offset - 2
        let pc = int32 machine.Pc
        let offset = int16 offset
        let newPc = pc + (int32 (offset - 2s))
        {machine with Pc = uint32 newPc }

    | OneOp,    0xD, [address] ->       // PRINT_PADDR
        p "PRINT_PADDR" |> d
        let sb = new System.Text.StringBuilder()
        let _,zscii = Text.toZChars ((uint32 address) * 2u) machine.Memory.ReadWord
        let text = Text.printZChars zscii machine.Memory machine.Header.AbbrevLoc (sb.Append >> ignore)
        machine.Write (sb.ToString())
        machine
  
    | OneOp,    0xE, [var] ->       // LOAD
        p "LOAD" |> d
        let v, m = machine.GetVariable (byte var)
        m.SetReturnValue v

    | OneOp,    0xF, [var] ->       // NOT
        p "NOT" |> d        
        machine.SetReturnValue (~~~var)

    // TWO OP

    | TwoOp,    0x1, x :: tail  ->   // JE
        p"JE" |> d
        machine.Branch(List.exists((=)x)tail) 
    
    | TwoOp,    0x2, [a;b] ->   // JL
        p"JL" |> d
        // signed comparison
        machine.Branch((int16 a) < (int16 b)) 
    
    | TwoOp,    0x3, [a;b] ->   // JG
        p"JG" |> d
        // signed comparison
        machine.Branch((int16 a) > (int16 b)) 
           
    | TwoOp,    0x4, [var;value] -> // DEC_CHK
        p"DEC_CHK" |> d
        let v, m = machine.GetVariable (byte var)
        let v =  (int16 v)-1s        
        (m.SetVariable (byte var) (uint16 v)).Branch(v < int16 value)

    | TwoOp,    0x5, [var;value] -> // INC_CHK
        p"INC_CHK" |> d        
        let v, m = machine.GetVariable (byte var)
        let v =  (int16 v)+1s        
        (m.SetVariable (byte var) (uint16 v)).Branch(v > int16 value)
 
    | TwoOp,    0x6, [obj1;obj2] ->   // JIN
        p"JIN" |> d
        let parent = machine.Objects.GetObjectParent machine.Memory (byte obj1) |> uint16
        machine.Branch((parent = obj2))

    | TwoOp,    0x7, [x;y]  ->   // TEST
        p"TEST" |> d
        machine.Branch (uint16(int16 x &&& int16 y) = (uint16 y))         
 
    | TwoOp,    0x8, [x;y]  ->   // OR
        p"OR" |> d
        machine.SetReturnValue (uint16(int16 x ||| int16 y))        
 
    | TwoOp,    0x9, [x;y]  ->   // AND
        p"AND" |> d
        machine.SetReturnValue (uint16(int16 x &&& int16 y))        

    | TwoOp,   0xA, [obj;att] ->  // TEST_ATTR
        p "TEST_ATTR" |> d
        machine.Branch(machine.Objects.GetObjectAttribute machine.Memory obj att)

    | TwoOp,   0xB, [obj;att] ->  // SET_ATTR
        p "SET_ATTR" |> d
        {machine with Memory = machine.Objects.SetObjectAttribute machine.Memory obj att true}

    | TwoOp,   0xC, [obj;att] ->  // CLEAR_ATTR
        p "CLEAR_ATTR" |> d
        {machine with Memory = machine.Objects.SetObjectAttribute machine.Memory obj att false}
    
    | TwoOp,   0xD, [var; value] ->  // STORE
        p "STORE" |> d
        machine.SetVariable (byte var) value

    | TwoOp,   0xE, [obj;dest] ->  // INSERT_OBJ
        p "INSERT_OBJ" |> d
        {machine with Memory = machine.Objects.InsertObject machine.Memory obj dest}

    | TwoOp,   0xF, [arr;index] ->  // LOADW
        p "LOADW" |> d
        let address = arr + 2us * index
        machine.SetReturnValue (machine.Memory.ReadWord (uint32 address))

    | TwoOp,   0x10, [arr;index] ->  // LOADB
        p "LOADB" |> d
        let address = arr + index
        machine.SetReturnValue (uint16 (machine.Memory.ReadByte (uint32 address)))
       
    | TwoOp,    0x11, [obj;prop]  ->   // GET_PROP
        p"GET_PROP" |> d
        machine.SetReturnValue(machine.Objects.GetObjectProperty machine.Memory obj prop)
    
    | TwoOp,    0x12, [obj;prop] ->  //GET_PROP_ADDR
        p"GET_PROP_ADDR" |> d
        machine.SetReturnValue(machine.Objects.GetObjectPropertyAddress machine.Memory obj prop)        

    | TwoOp,    0x13, [obj;prop]  ->   // GET_NEXT_PROP
        p"GET_NEXT_PROP" |> d
        machine.SetReturnValue(machine.Objects.GetObjectNextPropertyAddress machine.Memory obj prop)        
            
    | TwoOp,    0x14, [x;y]  ->   // ADD
        // signed
        p"ADD" |> d
        machine.SetReturnValue (uint16(int16 x + int16 y))
    
    | TwoOp,    0x16, [x;y]  ->   // MUL
        // signed
        p"MUL" |> d
        machine.SetReturnValue (uint16(int16 x * int16 y))
    
    | TwoOp,    0x17, [x;y]  ->   // DIV
        // signed
        p"DIV" |> d
        machine.SetReturnValue (uint16(int16 x / int16 y))

    | TwoOp,    0x18, [x;y]  ->   // MOD
        // signed
        p"MOD" |> d
        machine.SetReturnValue (uint16(int16 x % int16 y))

    | TwoOp,    21, [x;y]  ->   // SUB
         // signed
        p"SUB" |> d
        machine.SetReturnValue (uint16(int16 x - int16 y))

    // VAR OP
    | VarOp,    0x0, values ->    // CALL
        p"CALL" |> d
        if values.Length > 0 && values.Head = 0us then 
            machine.SetReturnValue 0us
        else
            machine.PushCall values

    | VarOp,    0x1, [arr;index;value] ->    // STOREW
        p "STOREW" |> d
        let address = arr + 2us * index
        {machine with Memory = machine.Memory.WriteWord (uint32 address) value}

    | VarOp,    0x2, [arr;index;value] ->    // STOREB
        p "STOREB" |> d
        let address = arr + index
        {machine with Memory = machine.Memory.WriteByte (uint32 address) (byte value)}
    
    | VarOp,    0x3, [obj; prop; value] ->    // PUT_PROP
        p "PUT_PROP" |> d
        { machine with Memory = machine.Objects.SetObjectProperty machine.Memory obj prop value }

    | VarOp,    0x4, [text; parse] ->    // SREAD
        p "SREAD" |> d
        let inputText = machine.Read()
        let machine, inputText = 
            if inputText.StartsWith("MARK ", System.StringComparison.CurrentCulture) then
                let rest = inputText.Replace("MARK ", "")            
                System.Console.WriteLine("You mark your progress via the power of the Squirrels")
                { machine with Markers = Map.add rest machine machine.Markers }, System.Console.ReadLine()
            elif inputText.StartsWith("RESTORE ", System.StringComparison.CurrentCulture) then
                let rest = inputText.Replace("RESTORE ", "")            
                System.Console.WriteLine("A Squirrel, mostly pink, transports you back through ZSpacetime ... !")
                { machine.Markers.[rest] with Markers = machine.Markers }, System.Console.ReadLine()
            else machine, inputText
        
        let memory = 
            Text.readInput 
                machine.Memory 
                (uint32 machine.Header.DictLoc) 
                machine.Header.AbbrevLoc 
                (uint32 text) 
                (uint32 parse)
                inputText

        {machine with Memory = memory }
  
    | VarOp,    0x5, [zchar] ->    // PRINT_CHAR
        p "PRINT_CHAR" |> d
        machine.Write (getZscii zchar |> string)
        machine

    | VarOp,    0x6, [num] ->    // PRINT_NUM
        p "PRINT_NUM" |> d
        machine.Write (num.ToString())
        machine

    | VarOp,    0x7, [num] ->    // RANDOM
        p "RANDOM" |> d // todo : this is wrong, fix it
        let v = 
            if num > 0us then
                let rnd = System.Random()
                uint16 <| rnd.Next(1,int32 num)
            else 
                let rnd = System.Random(int32 num)
                uint16 <| rnd.Next(1,int32 num)
        machine.SetReturnValue v

    | VarOp,    0x8, [value] ->    // PUSH
        p "PUSH" |> d
        machine.PushStack value

    | VarOp,    0x9, [value] ->    // PULL
        p "PULL" |> d
        //note until verison 6 this does not behave like you would expect and return a value
        //in the normal fashion (hence the parameter) intsead it writes the result to [value] !
        let v, m = machine.PopStack() 
        m.SetVariable (byte value) v


    | _ -> failwith "unrecognised opcode"