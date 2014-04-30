namespace InteractiveProvider.Interfaces

type IInteractiveState=
    abstract member DisplayText : string
    abstract member DisplayOptions : (string * int) list

type IInteractiveServer = 
    abstract member NewState : IInteractiveState
    abstract member ProcessResponse : IInteractiveState * int -> IInteractiveState