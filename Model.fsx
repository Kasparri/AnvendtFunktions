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
  new TextBox(Location=Point(50,30),Size=Size(120,50), Text="2")
let minBox = 
  new TextBox(Location=Point(175,30),Size=Size(120,50), Text="1")
let maxBox = 
  new TextBox(Location=Point(300,30),Size=Size(120,50), Text="8")

let fetchOKButton =
  new Button(Location=Point(50,70),MinimumSize=Size(250,50),
                MaximumSize=Size(250,50),Text="Fetching OK")
let cancelButton = 
  new Button(Location=Point(310,70),MinimumSize=Size(115,50),
               MaximumSize=Size(115,50),Text="Cancel load")

let loadButton = 
  new Button(Location=Point(265,10),MinimumSize=Size(50,25),
               MaximumSize=Size(50,25),Text="Load Game")


let slider =
  new TrackBar(Location=Point(20,40), Minimum=1, Maximum=40, 
                TickFrequency=1, SmallChange=1, LargeChange=5, Size=Size(200,50))
let sliderLabel =
  new Label(Location=Point(20,10), Text="Sticks to remove")
let sliderBox =
  new TextBox(Location=Point(150,10), Size=Size(25,25), Text=slider.Value.ToString())


let ansBox =
  new TextBox(Location=Point(150,150),Size=Size(200,25))

let clearButton = 
  new Button(Location=Point(575,650),MinimumSize=Size(100,25),
               MaximumSize=Size(100,25),Text="Clear Game")


let disable bs = 
    for b in [fetchButton;cancelButton;clearButton] do 
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


let checkBox s = 
    let mutable result = 0
    if Int32.TryParse(s, &result) && result > 0 then true
    else false

let checkBoxMax s max = 
    let mutable result = 0
    if Int32.TryParse(s, &result) && result > 0 && result <= max then true
    else false


type Message =
  | Take of (int*int) | Clear | Cancel | Web of string | Error | Cancelled | Load of (int*int*int)

let mutable sticks = [| |]

let mutable heapButtons:Button list= [ ]

let ev:AsyncEventQueue<Message> = AsyncEventQueue()

let victorycheck (A:int[]) =
    Array.forall (fun i -> i = 0) A

let makeArray (html:string) = 
    let trimmed = html.Trim()
    let array = trimmed.Split (' ', '\n')
    let finishedArray = Array.map int array
    ansBox.Text <- sprintf "%A" finishedArray
    finishedArray


let setButtonTexts() = for i = 0 to sticks.Length-1 do
                        let text = sprintf "%d" sticks.[i]
                        heapButtons.[i].Text <- text

let takeAction n i (a:(int [])) =
    if n > a.[i] then ansBox.Text <- "Can't take more sticks than there are"
    else 
          a.[i] <- (a.[i]-n)
    setButtonTexts()
    

let consURL n min max = sprintf "https://www.random.org/integers/?num=%d&min=%d&max=%d&col=1&base=10&format=plain&rnd=new" n min max
  
let getM a = Array.fold (fun s v -> s^^^v ) 0 a

let movePred ak m = (ak ^^^ m) < ak

let makeZeroMove array m =
    let id = Array.findIndex (fun ak -> movePred ak m ) array
    let diff = sticks.[id] - (sticks.[id] ^^^ m)
    takeAction diff id sticks
                     
let removeFromBiggest() = takeAction 1 (Array.findIndex (fun v -> v = (Array.max sticks)) sticks) sticks

let aiMove() = if (getM sticks) = 0 then removeFromBiggest() else makeZeroMove sticks (getM sticks)



let createHeapButtons() = 
                  for i = 0 to sticks.Length-1 do
                    let currentButton = new Button(Location=Point(25+(i%3*125) ,300 + ((i/3)*125)),MinimumSize=Size(100,100),MaximumSize=Size(100,100))
                    heapButtons <- currentButton::heapButtons
                    window.Controls.Add currentButton
                    currentButton.Click.Add (fun _ -> ev.Post ( Take (1,i)))
                  heapButtons <- List.rev heapButtons
                  setButtonTexts()

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
        
         disable [clearButton;fetchButton]
         let! msg = ev.Receive()
         match msg with
            | Web html -> sticks <- makeArray html
                          createHeapButtons()
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
        |Take (n,h) -> let before = Array.copy sticks
                       takeAction n h sticks
                       if before = sticks then return! player()
                       else
                            return! AI()
        |_ -> failwith("player: unexpected message")
    }
and AI() =
    async {
    if victorycheck sticks
    then return! finished("Player won")
    else disable [fetchButton;cancelButton]
         Console.WriteLine "AIs tur"
         aiMove()
         return! player()
    }
and finished(s) =
    async {
    ansBox.Text <- s
    disable [cancelButton]
    let! msg = ev.Receive()
    match msg with
        | Clear -> return! ready()
        | _     ->  failwith("finished: unexpected message")
    }

// Initialization

window.Controls.Add clearButton
window.Controls.Add ansBox

window.Controls.Add slider
window.Controls.Add sliderLabel
window.Controls.Add sliderBox
slider.Scroll.Add ( fun _ -> sliderBox.Text <- slider.Value.ToString() )




sliderBox.TextChanged.Add ( fun _ -> if checkBoxMax (sliderBox.Text) (slider.Maximum) 
                                     then slider.Value <- int sliderBox.Text
                                     else slider.Value <- slider.Value )




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


let checkFetchBoxes () =
    if (checkBoxMax heapBox.Text 9) && (checkBox minBox.Text) && (checkBox maxBox.Text)
     then ((int (minBox.Text)) < (int (maxBox.Text)))
     else false



                                   
fetchOKButton.Click.Add ( fun _ -> if checkFetchBoxes() 
                                   then ( ev.Post (Load( (int heapBox.Text) , (int minBox.Text) , (int maxBox.Text) ) ) ) 
                                        ( fetchWindow.Close() )
                                   else Console.WriteLine "failure" )


cancelButton.Click.Add ( fun _ -> ev.Post Cancel)

// Start
Async.StartImmediate (ready())

Application.Run(window) (* Mac *)
//window.Show() (* Windows *)




