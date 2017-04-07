module Machine

let p = sprintf
let d = System.Diagnostics.Debug.WriteLine

open Memory
open Objects
open System.IO

type OpForm =
    | Long
    | Short
    | Var
//    | Ext
                
type OperandCount =
   | ZeroOp
   | OneOp
   | TwoOp
   | VarOp

type OperandType =
    | Large   // 2 bytes
    | Small   // 1 byte
    | Variable// 1 bytes
    | Omitted

type OpCode = OpForm * OperandCount

type StackFrame =
    {
        LocalMemory : MemoryManager
        StackAddress : uint32
        ReturnAddress : uint32
    }

type Machine =
    {
        Pc : uint32
        Sp : uint32
        Header : Header
        CallStack : StackFrame list
        Stack : MemoryManager
        Memory : MemoryManager
        Objects : ObjectManager
        Write : string -> unit
        Read : unit -> string
        Markers : Map<string, Machine>
    }
    with
        member this.CurrentStackFrame = List.head this.CallStack
 
        member this.AdvanceByte() =
            this.Memory.ReadByte this.Pc, { this with Pc = this.Pc + 1u }
            
        member this.AdvanceWord() =
            this.Memory.ReadWord this.Pc, { this with Pc = this.Pc + 2u }

        member this.PushStack value =
            { this with Sp = this.Sp + 2u; Stack = this.Stack.PushWord value }

        member this.PopStack() =
            let v, s = this.Stack.PopWord()
            v, { this with Sp = this.Sp - 2u; Stack = s }
    
        member this.GetVariable address =
            //Variable number $00 refers to the top of the stack, $01 to $0f mean the local variables of 
            //the current routine and $10 to $ff mean the global variables.
            match address with
            | 0uy -> this.PopStack()
            | n when n < 0x10uy ->
                let address = (uint32 n)-1u
                this.CurrentStackFrame.LocalMemory.ReadWord (address * 2u), this
            | n -> 
                let address = uint32 <| n - 16uy
                // this is a word address, dont forget to unpack
                this.Memory.ReadWord((uint32 this.Header.GlobalLoc) + (address * 2u)), this

        member this.SetVariable address value =
            match address with
            | 0uy -> this.PushStack value // write to the global stack and increase the stack pointer                                
            | n when n < 0x10uy ->
                // write local variable
                let address = (uint32 n)-1u
                let frame = { (List.head this.CallStack) with LocalMemory = this.CurrentStackFrame.LocalMemory.WriteWord (address*2u) value }
                { this with CallStack = List.Cons (frame,List.tail this.CallStack) }
            | n ->
                // write global variable
                let address = uint32 <| n - 16uy
                let address = ((uint32 this.Header.GlobalLoc) + (address * 2u))
                { this with Memory = this.Memory.WriteWord address value }

        member this.SetReturnValue value =
            let address, this = this.AdvanceByte()
            this.SetVariable address value

        member this.PushCall (operands:uint16 list) =
            // save current location
            let ret = this.Pc
            let this = { this with Pc = (uint32 operands.[0]) * 2u }
            let sa = this.Sp
            let locals, this = this.AdvanceByte()
            
            // first up is to create the variable space for the call as determined
            // by the first operand
            let rec createLocals mem machine = function
                | n when n = locals -> mem, machine
                | n ->
                    let l, m = (machine:Machine).AdvanceWord()
                    createLocals (mem.PushWord l) m (n+1uy)

            let locals, this =
                createLocals (createMemoryManager Map.empty) this 0uy

            // now the rest of the operands themselves are written into the first few
            // blocks
            let locals,_ =
                ((locals,0u),List.tail operands)
                ||> List.fold(fun (m,i) v -> (m.WriteWord i v), i + 2u )

            let frame =
                { ReturnAddress = ret
                  StackAddress = sa
                  LocalMemory = locals }

            { this with CallStack = frame :: this.CallStack }
          
          member this.PopCall returnValue = 
            // restore Pc and Sp, pop the CallStack, and then store the result in 
            // the location provided by the next byte
            let (head :: tail) = this.CallStack
            let this = {this with Pc = head.ReturnAddress; Sp = head.StackAddress; CallStack = tail}
            this.SetReturnValue returnValue

          member this.Branch condition =
            let branchByte, this = this.AdvanceByte()
            // if bit 7 is 0 then branch when condition is false, else true
            let branch = branchByte  &&&= 0x80uy
            let condition =  if branch = false then not condition else condition
            let offset, this =
                if branchByte &&&= 0x40uy then
                    // if bit 6 is set, then the offset is a number between 0-63
                    // formed from bits 6-0 of the same byte
                    (uint32 (branchByte &&& 0x3Fuy)), this
                else
                    // otherwise this is a "signed" 14 bit number consisting of
                    // bits 5-0 followed by the next byte.
                    let temp = uint32(branchByte &&& 0x3Fuy)
                    let temp = if temp > 31u then temp - 64u else temp
                    let temp = temp <<< 8
                    let next, this = this.AdvanceByte()
                    temp ||| (uint32 next), this

            match condition, offset with
            | false, _               -> this                    // not branching
            | true, ((0u | 1u) as n) -> this.PopCall (uint16 n) // return false / true from current call
            | true, n -> { this  with Pc = this.Pc + n - 2u }   // branch to pc + offset - 1


let getNextInstruction (machine:Machine) =
    let (b1,machine) = machine.AdvanceByte()

    // decode opcode
    let form, count, number =
        match b1 with 
        | MaskEq 0xE0uy -> Var,   VarOp,  b1 &&& 0x1Fuy    
        | MaskEq 0xC0uy -> Var,   TwoOp,  b1 &&& 0x1Fuy            
        | MaskEq 0xB0uy -> Short, ZeroOp, b1 &&& 0xFuy 
        | MaskEq 0x80uy -> Short, OneOp,  b1 &&& 0xFuy
        | _ ->             Long,  TwoOp,  b1 &&& 0x1Fuy  

    // determine operand types
    let machine, types = 
        match form with
        | Short ->
            match b1 with
            | MaskEq 0x30uy -> machine, [Omitted]
            | MaskEq 0x20uy -> machine, [Variable]
            | MaskEq 0x10uy -> machine, [Small]
            | _ ->             machine, [Large] 
        | Long ->
            let f n = if b1 &&&= n then Variable else Small
            machine, [f 0x40uy ; f 0x20uy]
        | Var ->
            let (value,machine) = machine.AdvanceByte()
            let rec aux shift codes =
                if shift = -2 then codes else
                match value >>> shift with                    
                | MaskEq 0b11uy -> codes //omitted, stop here
                | MaskEq 0b10uy -> aux (shift-2) (Variable :: codes)
                | MaskEq 0b01uy -> aux (shift-2) (Small :: codes)
                | _ -> aux (shift-2) (Large::codes)                    
            machine, aux 6 [] |> List.rev 

    // load operands
    let machine, operands =
        ((machine,[]), types)
        ||> List.fold (fun (m,vs) c ->
            match c with
            | Large ->
                let (v, m) = m.AdvanceWord()
                (m, v :: vs)
            | Variable ->
                let (v, m) = m.AdvanceByte()
                let (v, m) = m.GetVariable v
                (m, v :: vs)                     
            | Small ->
                let (v, m) = m.AdvanceByte()
                (m,uint16 v :: vs)
            | Omitted ->
                (m,vs))

    machine, (count, int number, List.rev operands)


let fileRead file =
    let bytes = File.ReadAllBytes file
    [for x in 0 .. 2 ..  (bytes.Length - 1) do
         let b1 = (uint16 bytes.[x]) <<< 8
         let b2 =
             if x = bytes.Length - 1 then 0us
             else (uint16 bytes.[x+1])
         yield uint32 x, b1 ||| b2]
    |> Map.ofList
    |> createMemoryManager

let createMachine write read file =
    let mem = fileRead file
    let header = {
        Version = mem.ReadByte 0u
        HighLoc = mem.ReadWord 0x4u
        Pc = mem.ReadWord 0x6u 
        GlobalLoc = mem.ReadWord 0xCu
        DictLoc = mem.ReadWord 0x8u
        ObjLoc = mem.ReadWord 0xAu
        VarLoc = mem.ReadWord 0xCu
        StaticLoc = mem.ReadWord 0xEu
        AbbrevLoc = mem.ReadWord 0x18u }

    { Memory = mem
      Header = header
      Pc = uint32 header.Pc
      Sp = 0u
      CallStack = []
      Stack = createMemoryManager Map.empty
      Objects = createObjectManager header.ObjLoc header.AbbrevLoc
      Write = write
      Read = read
      Markers = Map.empty }
