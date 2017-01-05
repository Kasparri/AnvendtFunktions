// Applied Functional Programming 02257
// Project 1 - Nim Game - Model & Dialogue

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453

// Prelude

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing


#r "EventQueue.dll"
open EventQueue

#r "View.dll"
open View


(* Model *)

type Message =
  | Take of (int*int) | Clear 
  | Cancel | Web of string 
  | Error | Cancelled 
  | Load of (int*int*int) | Fetch
  | Generate | Create of (int*int*int)

let mutable sticks = Array.empty
let mutable taunted = false
let ev:AsyncEventQueue<Message> = AsyncEventQueue()

let checkBox s = 
    let mutable result = 0
    if Int32.TryParse(s, &result) && result > 0 then true
    else false

let checkBoxMax s max = 
    let mutable result = 0
    if Int32.TryParse(s, &result) && result > 0 && result <= max then true
    else false

let victorycheck A = Array.forall (fun i -> i = 0) A

let makeArray (html:string) = Array.map int ((html.Trim()).Split (' ', '\n'))

let consURL n min max = sprintf "https://www.random.org/integers/?num=%d&min=%d&max=%d&col=1&base=10&format=plain&rnd=new" n min max
  
let getM a = Array.fold (fun s v -> s^^^v ) 0 a

let movePred ak m = (ak ^^^ m) < ak

let difficultyCheck value = let rand = System.Random()
                            rand.Next (1,100) >= value

let randAmountHeap() = let rand = System.Random()
                       let index = rand.Next(1, ((sticks.Length)-1))
                       if sticks.[index] > 0 then (rand.Next (1,sticks.[index]),index)
                       else (0,0)


let generateGame h min max = 
    let mutable game = List.empty
    let rand = System.Random()
    if min >= 1 && max > min && h > 0 && h <= 9 then
        for i = 1 to h do 
            game <- (rand.Next(min,max))::game
    else
        for i = 1 to (rand.Next(1,9)) do
            game <- (rand.Next(1,20))::game
    List.toArray game


(* Dialogue *)

let checkFetchBoxes () =
    if (checkBoxMax heapBox.Text 9) && (checkBox minBox.Text) && (checkBox maxBox.Text)
     then ((int (minBox.Text)) < (int (maxBox.Text)))
     else false

let checkGenerateBoxes () =
    if (checkBoxMax gheapBox.Text 9) && (checkBox gminBox.Text) && (checkBox gmaxBox.Text)
     then ((int (gminBox.Text)) < (int (gmaxBox.Text)))
     else false

let mutable heapButtons:Button list = List.empty

let resetVariables () = sticks <- Array.empty
                        heapButtons <- List.empty
                        taunted <- false


let setButtonTexts() = for i = 0 to sticks.Length-1 do
                        heapButtons.[i].Text <- string sticks.[i]

let takeAction n i (a:(int [])) =
    if n > a.[i] then ansBox.Text <- "Can't take more sticks than there are"
    else a.[i] <- (a.[i]-n)
         setButtonTexts()
    

let makeZeroMove array m =
    if difficultyCheck difficultySlider.Value then 
        let id = Array.findIndex (fun ak -> movePred ak m ) array
        let diff = sticks.[id] - (sticks.[id] ^^^ m)
        if (not taunted) then ansBox.Text <- sprintf "you will lose :-)"
                              taunted <- true
        heapButtons.[id].BackColor <- Color.Red
        takeAction diff id sticks
    else
        if taunted then ansBox.Text <- "Oops"
                        taunted <- false
        let (amount,heap) = randAmountHeap()
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


let rec ready() = 
         async {
         disable [cancelButton;fetchOKButton;cancelFetchButton;
                  cancelGenerateButton;generateOKButton]
         let! msg = ev.Receive()
         match msg with
            | Clear    -> return! ready()
            | Fetch    -> return! fetching()
            | Generate -> return! generating()
            | _        -> failwith ("ready: unexpected message")
         }
and fetching() = 
          async {
          disable [clearButton;fetchButton;cancelButton;
                   cancelGenerateButton;generateOKButton;generateButton]
          let! msg = ev.Receive()
          match msg with
            | Clear  -> return! fetching()
            | Load (n,min,max)  -> return! loading(consURL n min max)
            | Cancel -> return! ready()
            | _      -> failwith ("loading:unexpected message")
         }
and generating() = 
          async {
          disable [clearButton;fetchButton;cancelButton;
                   fetchOKButton;cancelFetchButton;generateButton]
          let! msg = ev.Receive()
          match msg with
            | Clear  -> return! generating()
            | Create (h,min,max)  -> return! creating(h,min,max)
            | Cancel -> return! ready()
            | _      -> failwith ("loading:unexpected message")
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
        
         disable [clearButton;fetchButton;fetchOKButton;cancelFetchButton;
                  cancelGenerateButton;generateOKButton;generateButton]
         let! msg = ev.Receive()
         match msg with
            | Web html -> sticks <- makeArray html
                          createHeapButtons()
                          ansBox.Text <- "Loaded game from Random.org"
                          return! player()
            | Error -> sticks <- generateGame (int heapBox.Text) (int minBox.Text) (int maxBox.Text)
                       createHeapButtons()
                       ansBox.Text <- "Generated game offline"
                       return! player()
            | Cancel -> ts.Cancel()
                        return! cancelling()
            | _     -> failwith ("loading:unexpected message")

         }
and creating(h,min,max) =
  async {
           disable [fetchButton;fetchOKButton;cancelFetchButton;clearButton;
                    cancelGenerateButton;generateOKButton;cancelButton;generateButton]
           sticks <- generateGame h min max
           createHeapButtons()
           ansBox.Text <- "Generated game offline"
           return! player()
       }
and cancelling() = 
  async {
         
         disable [clearButton; cancelButton;cancelFetchButton;
                  cancelGenerateButton;generateOKButton;generateButton]
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
    disable [fetchButton;fetchOKButton;cancelButton;cancelFetchButton;
             cancelGenerateButton;generateOKButton;generateButton]

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
    async {
    if victorycheck sticks
    then return! finished("Player won")
    else disable [fetchButton;fetchOKButton;cancelFetchButton;clearButton;
                  cancelGenerateButton;generateOKButton;cancelButton;generateButton]
         aiMove()
         do! Async.Sleep 1000
         for i=0 to sticks.Length-1 do
            heapButtons.[i].ResetBackColor()
         return! player()
    }
and finished(s) =
    async {
    ansBox.Text <- s
    disable [cancelButton;fetchButton;fetchOKButton;cancelFetchButton;
             cancelGenerateButton;generateOKButton;generateButton]
    let! msg = ev.Receive()
    match msg with
        | Clear -> resetVariables()
                   return! ready()
        | _     ->  failwith("finished: unexpected message")
    }

// Initialization
(* Main window *)

fetchButton.Click.Add ( fun _ -> ev.Post Fetch
                                 fetchWindow.Show() )
generateButton.Click.Add ( fun _ -> ev.Post Generate
                                    generateWindow.Show() )  
cancelButton.Click.Add ( fun _ -> ev.Post Cancel )
clearButton.Click.Add ( fun _ -> ansBox.Text <- ""
                                 for i = 0 to (sticks.Length) - 1 do
                                   window.Controls.Remove heapButtons.[i]
                                 ev.Post Clear )

slider.Scroll.Add ( fun _ -> sliderBox.Text <- slider.Value.ToString() )
sliderBox.TextChanged.Add ( fun _ -> if checkBoxMax (sliderBox.Text) (slider.Maximum) 
                                     then ansBox.Text <- ""
                                          slider.Value <- int sliderBox.Text
                                     else ansBox.Text <- sprintf "Amount has to be between 1 and %d" slider.Maximum
                                          slider.Value <- slider.Value )


(* Fetch window *)

fetchOKButton.Click.Add ( fun _ -> if checkFetchBoxes() 
                                   then ( ev.Post (Load( (int heapBox.Text) , (int minBox.Text) , (int maxBox.Text) ) ) ) 
                                        ( slider.Maximum <- (int maxBox.Text) )
                                        ( fetchWindow.Hide() )
                                   else ev.Post Clear )
cancelFetchButton.Click.Add ( fun _ -> ev.Post Cancel
                                       fetchWindow.Hide() )

difficultySlider.Scroll.Add ( fun _ -> difficultySliderBox.Text <- toDifficulty difficultySlider.Value)

(* Generate window *)

generateOKButton.Click.Add ( fun _ -> if checkGenerateBoxes() 
                                      then ( ev.Post (Create( (int gheapBox.Text) , (int gminBox.Text) , (int gmaxBox.Text) ) ) ) 
                                           ( slider.Maximum <- (int gmaxBox.Text) )
                                           ( generateWindow.Hide() )
                                      else ev.Post Clear )
cancelGenerateButton.Click.Add ( fun _ -> ev.Post Cancel
                                          generateWindow.Hide() )

gdifficultySlider.Scroll.Add ( fun _ -> gdifficultySliderBox.Text <- toDifficulty gdifficultySlider.Value)


// Start the program
Async.StartImmediate (ready())

//Application.Run(window) (* Mac *)
window.Show() (* Windows *)