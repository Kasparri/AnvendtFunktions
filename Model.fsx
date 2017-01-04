// Applied Functional Programming 02257
// Project 1 - Nim Game - Model

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing

#r "EventQueue.dll"
open EventQueue

// view

let window = 
  new Form(Text="Nim Game", Size=Size(412,565), BackColor = Color.GhostWhite)

let clearButton = 
  new Button(Location=Point(250,70),MinimumSize=Size(100,50),
               MaximumSize=Size(50,50),Text="Clear Game")

let fetchWindow = new Form(Text="Fetching Window", Size=Size(500,300), 
                           BackColor = Color.GhostWhite, ControlBox = false)

let fetchButton =
  new Button(Location=Point(250,10),MinimumSize=Size(45,50),
               MaximumSize=Size(50,50),Text="Fetch Button")
       
let cancelButton = 
  new Button(Location=Point(300,10),MinimumSize=Size(45,50),
               MaximumSize=Size(45,50),Text="Cancel load")

let heapLabel = new Label(Location=Point(50,15), Text="Amount of heaps")
let minLabel = new Label(Location=Point(175,15), Text="Min number")
let maxLabel = new Label(Location=Point(300,15), Text="Max number")

let toDifficulty = function
    |i when i>75 -> "Very Easy"
    |i when i>=50 -> "Easy"
    |i when i>25 -> "Hard"
    |i when i>10 -> "Very Hard"
    |i -> "Impossible"


let difficultySlider =
  new TrackBar(Location=Point(150,200), Minimum=1, Maximum=100, 
                TickFrequency=1, SmallChange=1, LargeChange=5, Size=Size(200,50))
let difficultySliderLabel =
  new Label(Location=Point(140,155), Text="Difficulty:")
let difficultySliderBox =
  new TextBox(Location=Point(260,150), Size=Size(90,20), Text= toDifficulty difficultySlider.Value)

let heapBox = 
  new TextBox(Location=Point(50,30),Size=Size(120,50), Text="2")
let minBox = 
  new TextBox(Location=Point(175,30),Size=Size(120,50), Text="1")
let maxBox = 
  new TextBox(Location=Point(300,30),Size=Size(120,50), Text="8")

let fetchOKButton =
  new Button(Location=Point(50,70),MinimumSize=Size(290,50),
                MaximumSize=Size(290,50),Text="Fetching OK")

let cancelFetchButton =
    new Button(Location=Point(350,70), MinimumSize=Size(100,50),
                  MaximumSize=Size(100,50),Text="Cancel")


let slider =
  new TrackBar(Location=Point(10,40), Minimum=1, Maximum=40, 
                TickFrequency=1, SmallChange=1, LargeChange=5, Size=Size(200,50))
let sliderLabel =
  new Label(Location=Point(10,15), Text="Sticks to remove:")
let sliderBox =
  new TextBox(Location=Point(110,10), Size=Size(25,20), Text=slider.Value.ToString())


let ansBox =
  new TextBox(Location=Point(10,90),Size=Size(200,25))

let disable bs = 
    for b in [fetchButton;cancelButton;clearButton;fetchOKButton;cancelFetchButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false


// Model

let checkBox s = 
    let mutable result = 0
    if Int32.TryParse(s, &result) && result > 0 then true
    else false

let checkBoxMax s max = 
    let mutable result = 0
    if Int32.TryParse(s, &result) && result > 0 && result <= max then true
    else false

let checkFetchBoxes () =
    if (checkBoxMax heapBox.Text 9) && (checkBox minBox.Text) && (checkBox maxBox.Text)
     then ((int (minBox.Text)) < (int (maxBox.Text)))
     else false

type Message =
  | Take of (int*int) | Clear | Cancel | Web of string | Error | Cancelled | Load of (int*int*int) | Fetch

let mutable sticks = Array.empty
let mutable heapButtons:Button list = List.empty
let mutable taunted = false
let ev:AsyncEventQueue<Message> = AsyncEventQueue()

let resetVariables () = sticks <- Array.empty
                        heapButtons <- List.empty
                        taunted = false

let victorycheck A = Array.forall (fun i -> i = 0) A

let makeArray (html:string) = Array.map int ((html.Trim()).Split (' ', '\n'))

let setButtonTexts() = for i = 0 to sticks.Length-1 do
                        heapButtons.[i].Text <- string sticks.[i]

let takeAction n i (a:(int [])) =
    if n > a.[i] then ansBox.Text <- "Can't take more sticks than there are"
    else 
          a.[i] <- (a.[i]-n)
    setButtonTexts()
    

let consURL n min max = sprintf "https://www.random.org/integers/?num=%d&min=%d&max=%d&col=1&base=10&format=plain&rnd=new" n min max
  
let getM a = Array.fold (fun s v -> s^^^v ) 0 a

let movePred ak m = (ak ^^^ m) < ak

let difficultyCheck() = 
                        let rand = System.Random()
                        rand.Next (1,100) >= difficultySlider.Value

let randomHeap() = let rand = System.Random()
                   let index = rand.Next(1, ((sticks.Length)-1))
                   if sticks.[index] > 0 then (rand.Next (1,sticks.[index]),index)
                   else (0,0)


let makeZeroMove array m =
    if difficultyCheck() then 
        Console.WriteLine "Making a smart move"
        let id = Array.findIndex (fun ak -> movePred ak m ) array
        let diff = sticks.[id] - (sticks.[id] ^^^ m)
        if (not taunted) then ansBox.Text <- sprintf "you will lose ( ͠° ͟ʖ ͡°)"
                              taunted <- true
        heapButtons.[id].BackColor <- Color.Red
        takeAction diff id sticks
    else
        Console.WriteLine "Making a dumb move"
        if taunted then ansBox.Text <- "Oops"
                        taunted <- false
        let (amount,heap) = randomHeap()
        heapButtons.[heap].BackColor <- Color.Red
        takeAction amount heap sticks


                     
let removeFromBiggest() = let maxIndex = Array.findIndex (fun v -> v = (Array.max sticks)) sticks
                          heapButtons.[maxIndex].BackColor <- Color.Red
                          takeAction 1 maxIndex sticks

let aiMove() = if (getM sticks) = 0 then removeFromBiggest() else makeZeroMove sticks (getM sticks)

let createHeapButtons() = 
                  for i = 0 to sticks.Length-1 do
                    let currentButton = new Button(Location=Point(24+(i%3*125),150 + ((i/3)*125)),
                                         MinimumSize=Size(100,100), 
                                         MaximumSize=Size(100,100))
                    heapButtons <- currentButton::heapButtons
                    window.Controls.Add currentButton
                    currentButton.Click.Add (fun _ -> ev.Post ( Take (slider.Value,i)))
                  heapButtons <- List.rev heapButtons
                  setButtonTexts()

let rec ready() = async {
         Console.WriteLine "Ready()"

         disable [cancelButton;fetchOKButton;cancelFetchButton]
         let! msg = ev.Receive()
         match msg with
            | Clear -> return! ready()
            | Fetch -> return! fetching()
            | _     -> failwith ("ready: unexpected message")
         }
and fetching() = 
          Console.WriteLine "Fetching()"
          async {

          disable [clearButton;fetchButton;cancelButton]
          let! msg = ev.Receive()
          match msg with
            | Clear  -> return! fetching()
            | Load (n,min,max)  -> return! loading(consURL n min max)
            | Cancel -> return! ready()
            | _      -> failwith ("loading:unexpected message")
         }
and loading(url) =
         Console.WriteLine "Loading()"
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
        
         disable [clearButton;fetchButton;fetchOKButton;cancelFetchButton]
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
  Console.WriteLine "Cancelling()"
  async {
         
         disable [clearButton; cancelButton;cancelFetchButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")
         }
and player() = 
    Console.WriteLine "Player()"
    async {
    if victorycheck sticks then return! finished("AI won") 
    else
    disable [fetchButton;fetchOKButton;cancelButton;cancelFetchButton]

    let! msg = ev.Receive()
    match msg with
        |Clear -> resetVariables()
                  return! ready()
        |Take (n,h) -> let before = Array.copy sticks
                       takeAction n h sticks
                       if before = sticks then return! player()
                       else
                            ansBox.Text <- ""
                            return! AI()
        |_ -> failwith("player: unexpected message")
    }
and AI() =
    Console.WriteLine "Ai()"
    async {
    if victorycheck sticks
    then return! finished("Player won")
    else disable [fetchButton;cancelButton;fetchOKButton;clearButton]
         aiMove()
         do! Async.Sleep 1000
         for i=0 to sticks.Length-1 do
            heapButtons.[i].ResetBackColor()
         return! player()
    }
and finished(s) =
    Console.WriteLine "Finished()"
    async {
    ansBox.Text <- s
    disable [cancelButton;fetchButton;fetchOKButton;cancelFetchButton]
    let! msg = ev.Receive()
    match msg with
        | Clear -> resetVariables()
                   return! ready()
        | _     ->  failwith("finished: unexpected message")
    }

// Initialization

window.Controls.Add clearButton
window.Controls.Add ansBox
window.Controls.Add slider
window.Controls.Add sliderLabel
window.Controls.Add sliderBox

window.Controls.Add fetchButton
window.Controls.Add cancelButton



fetchWindow.Controls.Add heapBox
fetchWindow.Controls.Add minBox
fetchWindow.Controls.Add maxBox
fetchWindow.Controls.Add heapLabel
fetchWindow.Controls.Add minLabel
fetchWindow.Controls.Add maxLabel
fetchWindow.Controls.Add fetchOKButton
fetchWindow.Controls.Add cancelFetchButton
fetchWindow.Controls.Add difficultySlider
fetchWindow.Controls.Add difficultySliderLabel
fetchWindow.Controls.Add difficultySliderBox


slider.Scroll.Add ( fun _ -> sliderBox.Text <- slider.Value.ToString() )
sliderBox.TextChanged.Add ( fun _ -> if checkBoxMax (sliderBox.Text) (slider.Maximum) 
                                     then slider.Value <- int sliderBox.Text
                                     else slider.Value <- slider.Value )


fetchButton.Click.Add ( fun _ -> ev.Post Fetch
                                 fetchWindow.Show() )

cancelButton.Click.Add ( fun _ -> ev.Post Cancel )

clearButton.Click.Add ( fun _ -> ansBox.Text <- ""
                                 for i = 0 to (sticks.Length) - 1 do
                                   window.Controls.Remove heapButtons.[i]
                                 ev.Post Clear )


difficultySlider.Scroll.Add ( fun _ -> difficultySliderBox.Text <- toDifficulty difficultySlider.Value)

                              
fetchOKButton.Click.Add ( fun _ -> if checkFetchBoxes() 
                                   then ( ev.Post (Load( (int heapBox.Text) , (int minBox.Text) , (int maxBox.Text) ) ) ) 
                                        ( slider.Maximum <- (int maxBox.Text) )
                                        ( fetchWindow.Hide() )
                                   else ev.Post Clear )

cancelFetchButton.Click.Add ( fun _ -> ev.Post Cancel
                                       fetchWindow.Hide() )

// Start
Async.StartImmediate (ready())

Application.Run(window) (* Mac *)
//window.Show() (* Windows *)




