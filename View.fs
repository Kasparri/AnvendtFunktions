// Applied Functional Programming 02257
// Project 1 - Nim Game - View

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453


// Prelude

module View
open System.Windows.Forms 
open System.Drawing 

let toDifficulty = function
    |i when i>75 -> "Very Easy"
    |i when i>=50 -> "Easy"
    |i when i>25 -> "Hard"
    |i when i>10 -> "Very Hard"
    |i -> "Impossible"


(* Main window *)
let window = new Form(Text="Nim Game", Size=Size(412,565), BackColor = Color.GhostWhite)

let clearButton = new Button(Location=Point(250,70),MinimumSize=Size(50,50),
                             MaximumSize=Size(50,50),Text="Clear Game")
let fetchButton = new Button(Location=Point(250,10),MinimumSize=Size(50,50),
                             MaximumSize=Size(50,50),Text="Fetch Game")      
let cancelButton = new Button(Location=Point(305,10),MinimumSize=Size(50,50),
                              MaximumSize=Size(50,50),Text="Cancel Fetch")
let generateButton = new Button(Location=Point(305,70),MinimumSize=Size(50,50),
                              MaximumSize=Size(50,50),Text="Gen. Game")

let slider = new TrackBar(Location=Point(10,40), Minimum=1, Maximum=40, Size=Size(200,50), 
                          TickFrequency=1, SmallChange=1, LargeChange=5)
let sliderLabel = new Label(Location=Point(10,15), Text="Sticks to remove:")
let sliderBox = new TextBox(Location=Point(110,10), Size=Size(25,20), Text=slider.Value.ToString())


let ansBox = new TextBox(Location=Point(10,90),Size=Size(200,25))



(* Fetch window *)
let fetchWindow = new Form(Text="Fetching Window", Size=Size(500,300), 
                           BackColor = Color.GhostWhite, ControlBox = false)

let heapLabel = new Label(Location=Point(50,15), Text="Amount of heaps")
let minLabel  = new Label(Location=Point(175,15), Text="Min number")
let maxLabel  = new Label(Location=Point(300,15), Text="Max number")

let heapBox = new TextBox(Location=Point(50,30),Size=Size(120,50), Text="2")
let minBox  = new TextBox(Location=Point(175,30),Size=Size(120,50), Text="1")
let maxBox  = new TextBox(Location=Point(300,30),Size=Size(120,50), Text="8")

let fetchOKButton = new Button(Location=Point(50,70),MinimumSize=Size(290,50),
                               MaximumSize=Size(290,50),Text="Fetching OK")
let cancelFetchButton = new Button(Location=Point(350,70), MinimumSize=Size(100,50),
                                   MaximumSize=Size(100,50),Text="Cancel")

let difficultySlider =
  new TrackBar(Location=Point(150,200), Minimum=1, Maximum=100, 
                TickFrequency=1, SmallChange=1, LargeChange=5, Size=Size(200,50))
let difficultySliderLabel =
  new Label(Location=Point(140,155), Text="Difficulty:")
let difficultySliderBox =
  new TextBox(Location=Point(260,150), Size=Size(90,20), Text= toDifficulty difficultySlider.Value)



(* Generate window *)
let generateWindow = new Form(Text="Generate Window", Size=Size(500,300), 
                           BackColor = Color.GhostWhite, ControlBox = false)

let gheapLabel = new Label(Location=Point(50,15), Text="Amount of heaps")
let gminLabel  = new Label(Location=Point(175,15), Text="Min number")
let gmaxLabel  = new Label(Location=Point(300,15), Text="Max number")

let gheapBox = new TextBox(Location=Point(50,30),Size=Size(120,50), Text="2")
let gminBox  = new TextBox(Location=Point(175,30),Size=Size(120,50), Text="1")
let gmaxBox  = new TextBox(Location=Point(300,30),Size=Size(120,50), Text="8")

let generateOKButton = new Button(Location=Point(50,70),MinimumSize=Size(290,50),
                               MaximumSize=Size(290,50),Text="Generate OK")
let cancelGenerateButton = new Button(Location=Point(350,70), MinimumSize=Size(100,50),
                                   MaximumSize=Size(100,50),Text="Cancel")

let gdifficultySlider =
  new TrackBar(Location=Point(150,200), Minimum=1, Maximum=100, 
                TickFrequency=1, SmallChange=1, LargeChange=5, Size=Size(200,50))
let gdifficultySliderLabel =
  new Label(Location=Point(140,155), Text="Difficulty:")
let gdifficultySliderBox =
  new TextBox(Location=Point(260,150), Size=Size(90,20), Text= toDifficulty difficultySlider.Value)


let disable bs = 
    for b in [fetchButton;cancelButton;clearButton;fetchOKButton;generateButton
              cancelFetchButton;generateOKButton;cancelGenerateButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

// Initialization

window.Controls.Add clearButton
window.Controls.Add ansBox
window.Controls.Add slider
window.Controls.Add sliderLabel
window.Controls.Add sliderBox
window.Controls.Add fetchButton
window.Controls.Add cancelButton
window.Controls.Add generateButton

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

generateWindow.Controls.Add gheapBox
generateWindow.Controls.Add gminBox
generateWindow.Controls.Add gmaxBox
generateWindow.Controls.Add gheapLabel
generateWindow.Controls.Add gminLabel
generateWindow.Controls.Add gmaxLabel
generateWindow.Controls.Add generateOKButton
generateWindow.Controls.Add cancelGenerateButton
generateWindow.Controls.Add gdifficultySlider
generateWindow.Controls.Add gdifficultySliderLabel
generateWindow.Controls.Add gdifficultySliderBox