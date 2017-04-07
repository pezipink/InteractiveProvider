module Memory

type Header =
    {
        Version : byte
        HighLoc : uint16
        Pc : uint16
        GlobalLoc : uint16
        DictLoc : uint16
        ObjLoc : uint16
        VarLoc : uint16
        StaticLoc : uint16
        AbbrevLoc : uint16
    }

type MemoryManager =
    {
        ReadByte : uint32 -> byte
        ReadWord : uint32 -> uint16
        WriteByte : uint32 -> byte -> MemoryManager
        WriteWord : uint32 -> uint16 -> MemoryManager        
        PushWord : uint16 -> MemoryManager
        PopWord : unit ->  uint16 * MemoryManager
#if DEBUG
        Memory :  Map<uint32, uint16>
#endif
    }
   

let rec createMemoryManager (memory:Map<uint32,uint16>) =
    let readByte loc =
        // left memory slot, shift  
        if loc % 2u = 0u then memory.[loc] >>> 8 
        // right memory slot, mask out directly
        else memory.[loc-1u] &&& 0xFFus 
        |> byte

    let readWord loc =
        if loc % 2u = 0u then memory.[loc]
        // we need the right 8 bits from byte 2 of address one
        // combine with the left 8 bits of address 2
        else (memory.[loc-1u] &&& 0xFFus) <<< 8 ||| (memory.[loc+1u] >>> 8)

    let writeByte loc (value:byte) =
        // left memory slot
        if loc % 2u = 0u then
            // shift new values in and mask together with existing low byte
            let temp = (uint16 value) <<< 8 ||| (memory.[loc] &&& 0xFFus)
            Map.add loc temp memory |> createMemoryManager
        else
            let temp = (memory.[loc-1u] &&& 0xFF00us) ||| (uint16 value)
            Map.add (loc-1u) temp memory |> createMemoryManager

    let writeWord loc value =
        if loc % 2u = 0u then
            Map.add loc value memory |> createMemoryManager
        else
            let left = writeByte loc (byte(value >>> 8 &&& 0xFFus))
            left.WriteByte (loc+1u) (byte value &&& 0xFFuy)

    let pushWord value =
        // could store this max instead...
        if memory.IsEmpty then createMemoryManager(Map.add 0u value memory)
        else
        let max = (0u,memory) ||> Map.fold(fun v k _ -> max v k)
        createMemoryManager (Map.add (max+2u) value memory)

    let popWord value =
        // could store this max instead...
        let max = (0u,memory) ||> Map.fold(fun v k _ -> max v k)
        memory.[max], createMemoryManager (Map.filter(fun k _ -> k <> max) memory)

    { ReadByte = readByte
      ReadWord = readWord
      WriteByte = writeByte
      WriteWord = writeWord
      PushWord = pushWord
      PopWord = popWord
#if DEBUG
      Memory = memory 
#endif      
      }
