[<AutoOpen>]
module Helpers

let inline (>>>=) input shift = input >>> shift &&& 1uy = 1uy
let inline (>>>!) input shift = input >>> shift &&& 1uy = 0uy

let inline (&&&=) input mask = input &&& mask = mask
let inline (&&&!) input mask = input &&& mask <> mask

let inline (|MaskEq|_|) mask input =
    if input &&& mask = mask then Some() else None
