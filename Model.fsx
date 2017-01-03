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
  new Form(Text="Nim Game", Size=Size(600,600))

let fetchWindow =
  new Form(Text="Fetching Window", Size=Size(500,200))

let fetchButton =
  new Button(Location=Point(450,450),MinimumSize=Size(50,50),
               MaximumSize=Size(50,50),Text="Fetch Button")

let heapLabel = new Label(Location=Point(50,15), Text="Amount of heaps")
let minLabel = new Label(Location=Point(175,15), Text="Min number")
let maxLabel = new Label(Location=Point(300,15), Text="Max number")

let heapBox = 
  new TextBox(Location=Point(50,30),Size=Size(125,50), Text="2")
let minBox = 
  new TextBox(Location=Point(175,30),Size=Size(125,50), Text="1")
let maxBox = 
  new TextBox(Location=Point(300,30),Size=Size(125,50), Text="8")

let fetchOKButton =
  new Button(Location=Point(50,70),MinimumSize=Size(250,50),
                MaximumSize=Size(250,50),Text="Fetching OK")
let cancelButton = 
  new Button(Location=Point(310,70),MinimumSize=Size(115,50),
               MaximumSize=Size(115,50),Text="Cancel load")

let loadButton = 
  new Button(Location=Point(265,10),MinimumSize=Size(50,25),
               MaximumSize=Size(50,25),Text="Load Game")


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
    for b in [fetchButton;takeButton;cancelButton;clearButton] do 
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

let mutable sticks = [| |]

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
  
let getM a = Array.fold (fun s v -> s^^^v ) 0 a

let movePred ak m = (ak ^^^ m) < ak

let makeZeroMove array m =
    let id = Array.findIndex (fun ak -> movePred ak m ) array
    let diff = sticks.[id] - sticks.[id] ^^^ m
    takeAction diff id sticks

                     
let removeFromBiggest() = takeAction 1 (Array.findIndex (fun v -> v = (Array.max sticks)) sticks) sticks

let aiMove() = if (getM sticks) = 0 then removeFromBiggest() else makeZeroMove sticks (getM sticks)

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
        
         disable [takeButton;clearButton;fetchButton]
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
    if victorycheck sticks then return! finished("AI won") 
    else
    disable [fetchButton]

    let! msg = ev.Receive()
    match msg with
        |Clear -> return! ready()
        |Take (n,h) -> takeAction n h sticks
                       ansBox.Text <- sprintf "%A" sticks
                       return! AI()
        |_ -> failwith("player: unexpected message")
    }
and AI() =
    async {
    if victorycheck sticks
    then return! finished("Player won")
    else disable [fetchButton;cancelButton]
         aiMove()
         return! player()
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

window.Controls.Add takeButton
window.Controls.Add clearButton
window.Controls.Add ansBox
window.Controls.Add heapNumberBox
window.Controls.Add amountBox


takeButton.Click.Add (fun _ -> ev.Post (Take ((int amountBox.Text),(int heapNumberBox.Text))) )
clearButton.Click.Add (fun _ -> ev.Post Clear)


window.Controls.Add fetchButton
fetchButton.Click.Add ( fun _ -> fetchWindow.Show() )

fetchWindow.Controls.Add heapBox
fetchWindow.Controls.Add minBox
fetchWindow.Controls.Add maxBox
fetchWindow.Controls.Add heapLabel
fetchWindow.Controls.Add minLabel
fetchWindow.Controls.Add maxLabel
fetchWindow.Controls.Add fetchOKButton
fetchWindow.Controls.Add cancelButton

fetchOKButton.Click.Add ( fun _ -> ev.Post (Load(int heapBox.Text, int minBox.Text,int maxBox.Text))
                                   fetchWindow.Close() )
cancelButton.Click.Add ( fun _ -> ev.Post Cancel)

// Start
Async.StartImmediate (ready())

//Application.Run(window) (* Mac *)
window.Show() (* Windows *)




