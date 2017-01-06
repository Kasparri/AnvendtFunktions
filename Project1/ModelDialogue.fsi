module ModelDialogue
//Model
type Message
val makeArray : html:string -> int []
val getM : a:int [] -> int
val movePred : ak:int -> m:int -> bool
val difficultyCheck : value:int -> bool
val generateGame : h:int -> min:int -> max:int -> int []

//Dialogue
val takeAction : n:int -> i:int -> a:int [] -> unit
val makeZeroMove : array:int [] -> m:int -> unit
val removeFromBiggest : unit -> unit
val aiMove : unit -> unit
    //States
val ready : unit -> Async<unit>
val fetching : unit -> Async<unit>
val generating : unit -> Async<unit>
val loading : url:string -> Async<unit>
val creating : h:int * min:int * max:int -> Async<unit>
val cancelling : unit -> Async<unit>
val player : unit -> Async<unit>
val AI : unit -> Async<unit>
val finished : s:string -> Async<unit>