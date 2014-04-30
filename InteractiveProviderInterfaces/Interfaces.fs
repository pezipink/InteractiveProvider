namespace InteractiveProvider.Interfaces

type IInteractiveState=
    abstract member DisplayText : string
    abstract member DisplayOptions : (string * obj) list

type IInteractiveServer = 
    abstract member NewState : IInteractiveState
    abstract member ProcessResponse : IInteractiveState * obj -> IInteractiveState