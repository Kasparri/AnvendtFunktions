// Applied Functional Programming 02257
// Project 1 - Nim Game - Model

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing

// view

let window = 
  new Form(Text="Nim Game", Size=Size(700,700))

let rowBox = 
  new TextBox(Location=Point(10,10),Size=Size(80,25), Text="Heap number")

let minBox = 
  new TextBox(Location=Point(95,10),Size=Size(80,25), Text="Min amount")

let maxBox = 
  new TextBox(Location=Point(180,10),Size=Size(80,25), Text="Max amount")

let loadButton = 
  new Button(Location=Point(265,10),MinimumSize=Size(50,25),
               MaximumSize=Size(50,25),Text="Load Game")

let cancelButton = 
  new Button(Location=Point(320,10),MinimumSize=Size(50,25),
               MaximumSize=Size(50,25),Text="Cancel load")

let ansBox =
  new TextBox(Location=Point(150,150),Size=Size(200,25))

let takeButton = 
  new Button(Location=Point(575,10),MinimumSize=Size(100,75),
               MaximumSize=Size(100,75),Text="Take!")

let clearButton = 
  new Button(Location=Point(575,650),MinimumSize=Size(100,25),
               MaximumSize=Size(100,25),Text="Clear Game")

let amountBox =
  new TextBox(Location=Point(575,100),Size=Size(80,25),Text="amount remove")

let heapNumberBox =
  new TextBox(Location=Point(575,130),Size=Size(80,25),Text="heap number")


let disable bs = 
    for b in [loadButton;takeButton;cancelButton;clearButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false




// Model

type AsyncEventQueue<'T>() = 
    let mutable cont = None 
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger() = 
        match queue.Count, cont with 
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d -> 
            cont <- None
            d (queue.Dequeue())

    let tryListen(d) = 
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() = 
        Async.FromContinuations (fun (cont,econt,ccont) -> 
            tryListen cont)



type Message =
  | Take of (int*int) | Clear | Cancel | Web of string | Error | Cancelled | Load of (int*int*int)

let mutable sticks = [| 5;4;3 |]

let ev:AsyncEventQueue<Message> = AsyncEventQueue()

let victorycheck (A:int[]) =
    Array.forall (fun i -> i = 0) A

let makeArray (html:string) = 
    let trimmed = html.Trim()
    let array = trimmed.Split (' ', '\n')
    let finishedArray = Array.map int array
    ansBox.Text <- sprintf "%A" finishedArray
    finishedArray




let removeSticks n i (a:(int [])) = 
    if n > a.[i] then failwith("Can't take more sticks than there are")
    else a.[i] <- (a.[i]-n)

let takeAction n h (A:(int [])) =
    removeSticks n h A
    ansBox.Text <- sprintf "%A" A

let consURL n min max = sprintf "https://www.random.org/integers/?num=%d&min=%d&max=%d&col=1&base=10&format=plain&rnd=new" n min max
   

let getM a =
    Array.fold (fun s v -> s^^^v ) 0 a

let makeZeroMove =
    let temp = Array.copy sticks
    for i in sticks do
        for j in 1..i do
            //removeSticks j i temp
            if getM temp = 0 then takeAction j i sticks

    
let removeFromBiggest = takeAction 1 (Array.max sticks) sticks

let aiMove a = if (getM a) = 0 then removeFromBiggest else makeZeroMove


let rec ready() = async {
         let! msg = ev.Receive()
         match msg with
            | Clear -> return! ready()
            | Load (n,min,max) -> return! loading(consURL n min max)
            | _     -> failwith ("ready: unexpected message")

         }

and loading(url) =
         async {
         use ts = new CancellationTokenSource()

         Async.StartWithContinuations
             (async { let webCl = new WebClient()
                      let! html = webCl.AsyncDownloadString(Uri url)
                      return html },
              (fun html -> ev.Post (Web html)),
              (fun _ -> ev.Post Error),
              (fun _ -> ev.Post Cancelled),
              ts.Token)
        
         disable [takeButton;clearButton;loadButton]
         let! msg = ev.Receive()
         match msg with
            | Web html -> sticks <- makeArray html
                          return! player()
            | Error -> return! finished("Error")
            | Cancel -> ts.Cancel()
                        return! cancelling()
            | _     -> failwith ("loading:unexpected message")

         }
and cancelling() = 
  async {
         
         disable [clearButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")
         }
and player() = 
    async {
    if victorycheck sticks then return! finished("Player won") 
    else
    disable [loadButton]

    let! msg = ev.Receive()
    match msg with
        |Clear -> return! ready()
        |Take (n,h) -> takeAction n h sticks
                       return! AI()
        |_ -> failwith("player: unexpected message")
    }
and AI() =
    async {
    if victorycheck sticks then return! finished("AI won")
    else
    disable [loadButton;cancelButton]

    let! msg = ev.Receive()
    match msg with
        |Take (n,h) -> takeAction n h sticks
                       return! player()
        |Clear -> return! ready()
        |_ -> failwith("AI: unexpected message")
    }
and finished(s) =
    async {
    ansBox.Text <- s
    disable [takeButton;cancelButton]
    let! msg = ev.Receive()
    match msg with
        | Clear -> return! ready()
        | _     ->  failwith("finished: unexpected message")
    }





// Initialization


window.Controls.Add minBox
window.Controls.Add maxBox
window.Controls.Add rowBox
window.Controls.Add loadButton
window.Controls.Add cancelButton
window.Controls.Add takeButton
window.Controls.Add clearButton
window.Controls.Add ansBox
window.Controls.Add heapNumberBox
window.Controls.Add amountBox

takeButton.Click.Add (fun _ -> ev.Post (Take ((int amountBox.Text),(int heapNumberBox.Text))) )

clearButton.Click.Add (fun _ -> ev.Post Clear)

cancelButton.Click.Add ( fun _ -> ev.Post Cancel)

loadButton.Click.Add ( fun _ -> ev.Post (Load(int rowBox.Text, int minBox.Text,int maxBox.Text)))


// Start
Async.StartImmediate (ready())

Application.Run(window) (* Mac *)
//window.Show() (* Windows *)


