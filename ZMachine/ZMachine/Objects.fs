module Objects

open Memory
open Text

type Object = 
    {
        Attributes : uint32
        Parent : byte
        Sibling : byte
        Child : byte
        Name : string
        Properties : Map<uint16, (uint32 * byte list)>
    }
        
type ObjectManager =
    {
        GetDefaultProperty : MemoryManager -> uint16 -> uint16
        GetObject : MemoryManager -> uint16 -> Object
        GetObjectProperty : MemoryManager -> uint16 -> uint16 -> uint16 
        GetObjectPropertyAddress : MemoryManager -> uint16 -> uint16 -> uint16 
        GetObjectNextPropertyAddress : MemoryManager -> uint16 -> uint16 -> uint16         
        SetObjectProperty : MemoryManager -> uint16 -> uint16 -> uint16 -> MemoryManager
        GetObjectSibling : MemoryManager -> byte -> byte
        GetObjectChild : MemoryManager -> byte -> byte
        GetObjectParent : MemoryManager -> byte -> byte
        GetObjectAttribute : MemoryManager -> uint16 -> uint16 -> bool 
        GetPropertyLength : MemoryManager -> uint16 -> uint16
        SetObjectAttribute : MemoryManager -> uint16 -> uint16 -> bool -> MemoryManager
        InsertObject : MemoryManager -> uint16 -> uint16 -> MemoryManager
        RemoveObject : MemoryManager -> uint16 -> MemoryManager
    }

let createObjectManager (objLoc:uint16) (abbrevLoc:uint16) =
    let treeStart= objLoc + 31us * 2us
    let getDefaultProp mem prop = 
        mem.ReadWord (uint32(objLoc + (prop-1us) * 2us))

    let getObject mem obj =
        // each object takes up 9 bytes        
        let offset = uint32 <| treeStart + (obj-1us) * 9us
        let propTable = mem.ReadWord(offset+7u)
        let textLength = mem.ReadByte (uint32 propTable)
        // ignore text length since we will wastefully (ha!) read the string in each time
        let sb = System.Text.StringBuilder()
        let endAddress, zchars = toZChars (uint32(propTable+1us)) mem.ReadWord 
        printZChars zchars mem abbrevLoc (sb.Append >> ignore)
   
        let rec readProps current props =            
            let sizeByte = mem.ReadByte current
            if sizeByte = 0uy then props else
            let propId = sizeByte &&& 31uy
            let propLen = (sizeByte / 32uy) + 1uy
            readProps 
                ((current + (uint32 propLen)) + 1u)
                ((uint16 propId, (current, [for x in 0u .. (uint32 (propLen-1uy)) -> mem.ReadByte (current+x+1u)])) :: props)

        let props = 
            readProps (endAddress+2u) []
            |> Map.ofList
        { 
            Attributes = (uint32(mem.ReadWord offset) <<< 16) ||| (uint32(mem.ReadWord (offset+2u)))
            Parent = mem.ReadByte (offset+4u)
            Sibling = mem.ReadByte (offset+5u)
            Child = mem.ReadByte (offset+6u)
            Name = sb.ToString()
            Properties = props
        }

    let getObjectAttribute mem obj attr =
        let obj = getObject mem obj
        // attributes are stored msb first so bit 7 of byte 4 is 0 and bit 0 of byte 0 is F
        let offset = int32 <| 31us - attr
        let mask = 1u <<< offset
        obj.Attributes &&& mask <> 0u

    let setObjectAttribute mem obj attr v =
        let objOffset = uint32 <| treeStart + (obj-1us) * 9us
        let obj = getObject mem obj        
        // attributes are stored msb first so bit 7 of byte 4 is 0 and bit 0 of byte 0 is F
        let offset = int32 <| 31us - attr
        let mask = 1u <<< offset
        let newAttributes = 
            if v then obj.Attributes ||| mask
            else obj.Attributes &&& (~~~mask)
        let mem = mem.WriteWord objOffset (uint16 (newAttributes >>> 16))
        mem.WriteWord (objOffset+2u) (uint16 (newAttributes &&& 0xFFFFu))

    // GET_PROP_ADDR
    let getObjectPropertyAddress mem obj prop  =
        let obj = getObject mem obj
        match Map.tryFind prop obj.Properties with
        | Some (sizeLoc, _) -> uint16 (sizeLoc+1u)
        | None -> 0us

    let getObjectNextPropertyAddress mem obj prop  =
        let obj = getObject mem obj
        let props = obj.Properties |> Map.toList |> List.sortByDescending fst
        if prop = 0us then props.Head |> fst 
        else
        let rec getNext = function
            | (current,data) :: (target,_) :: _ when prop = current -> target
            | [] -> 0us
            | _ :: tail -> getNext tail
        getNext props

    // GET_PROP
    let getObjectProperty mem obj prop  =
        // only properties with 1 - 2 bytes can be used with set / get - others are used 
        // for direct addressing
        let obj = getObject mem obj
        match Map.tryFind prop obj.Properties with
        | Some (_, v) when v.Length > 2 -> failwith "GET_PROP can only be used on properties of 1 - 2 bytes"
        | Some (_,v) -> 
            if v.Length = 2 then ((uint16 v.[0]) <<< 8) ||| (uint16 v.[1])
            else uint16 v.[0]            
        | None -> getDefaultProp mem prop

    // PUT_PROP
    let setObjectProperty mem obj prop value =
        // only properties with 1 - 2 bytes can be used with set / get - others are used 
        // for direct addressing
        let obj = getObject mem obj
        match Map.tryFind prop obj.Properties with
        | Some (_, v) when v.Length > 2 -> failwith "PUT_PROP can only be used on properties of 1 - 2 bytes"
        | Some (sizeLoc,v) -> 
            if v.Length = 2 then mem.WriteWord (sizeLoc+1u) value            
            else mem.WriteByte (sizeLoc+1u) (byte (value &&& 0xFFus))
        | None -> failwith "PUT_PROP cannot be called on properties that do not exist"

    let setParent mem (srcId:byte) (destId:byte) =
        let offset = uint32 <| treeStart + (uint16(srcId-1uy)) * 9us
        mem.WriteByte (offset+4u) destId
    
    let getParent mem (srcId:byte) =
        let offset = uint32 <| treeStart + (uint16(srcId-1uy)) * 9us
        mem.ReadByte (offset+4u)
     
    let setSibling mem (srcId:byte) (destId:byte) =
        let offset = uint32 <| treeStart + (uint16(srcId-1uy)) * 9us
        mem.WriteByte (offset+5u) destId
    
    let getSibling mem (srcId:byte) =
        let offset = uint32 <| treeStart + (uint16(srcId-1uy)) * 9us
        mem.ReadByte (offset+5u)

    let setChild mem (srcId:byte) (destId:byte) =
        let offset = uint32 <| treeStart + (uint16(srcId-1uy)) * 9us
        mem.WriteByte (offset+6u) destId
 
    let getChild mem (srcId:byte) =
        let offset = uint32 <| treeStart + (uint16(srcId-1uy)) * 9us
        mem.ReadByte (offset+6u)
    
    let getPropertyLength mem (propertyAddress:uint16) =
        let sizeByte = mem.ReadByte (uint32 (propertyAddress-1us))
        uint16 <| (sizeByte / 32uy) + 1uy

    let removeObject mem srcId =
        // detach the source object from its parent
        // its up to the game to patch up any problems this causes (OR IS IT!???)
        setParent mem (byte srcId) 0uy
           
    let insertObject mem srcId destId =
        // the source object becomes the first child of the destination.
        // any child already existing there becomes the soruce's new sibling.
        // subtrees from the source remain intact.
        let src = getObject mem srcId
        let dest = getObject mem destId
        let srcId = byte srcId
        let destId = byte destId
        //first, deal with the old parent. we must remove the child from the 
        // child / child's sibling tree
        let mem = 
            if src.Parent <> 0uy then
                let parent = getObject mem (uint16 src.Parent)
                // cycle through the parent's child's siblings, 
                // skipping the source object and "repairing" subsequent 
                // sibling link 

                //special case if the parent's immediate child is the source,
                //promote the first sibling to the new child and we are done
                if parent.Child = srcId then setChild mem src.Parent src.Sibling 
                else 
                let rec aux (current:byte) =
                    let next = getSibling mem current
                    if next = 0uy then failwith "did not find expected sibling to patch"
                    if next = srcId then
                        // set the current sibling to the source's sibling
                        setSibling mem  current src.Sibling
                    else aux next
                aux parent.Child
            else mem
        
        // now we can move the source into the first position of the new parent.
        let mem = setParent mem srcId destId
        let mem = setChild mem destId srcId 
        let mem = setSibling mem srcId dest.Child

        let src = getObject mem (uint16 srcId)
        let dest = getObject mem (uint16 destId)

        mem
    {
        GetDefaultProperty = getDefaultProp
        GetObject = getObject
        GetObjectProperty = getObjectProperty
        GetObjectPropertyAddress = getObjectPropertyAddress
        GetObjectNextPropertyAddress = getObjectNextPropertyAddress
        SetObjectProperty = setObjectProperty
        GetObjectAttribute = getObjectAttribute
        SetObjectAttribute = setObjectAttribute
        GetObjectParent = getParent
        GetObjectSibling = getSibling
        GetObjectChild = getChild
        InsertObject = insertObject
        RemoveObject = removeObject
        GetPropertyLength = getPropertyLength
    }