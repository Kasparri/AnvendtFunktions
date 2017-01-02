// Applied Functional Programming 02257
// Project 1 - Nim Game - View

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453


// Prelude
open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 

let window = new Form(Text="Nim Game", Size=Size(700,700))

let urlBox = new TextBox(Location=Point(10,10),Size=Size(400,25))

let ansBox =
  new TextBox(Location=Point(150,150),Size=Size(200,25))



let loadButton = new Button(Location=Point(415,10),MinimumSize=Size(50,25),
                  MaximumSize=Size(50,25),Text="Load Game")

let cancelButton = new Button(Location=Point(470,10),MinimumSize=Size(50,25),
                    MaximumSize=Size(50,25),Text="Cancel load")


let takeButton = new Button(Location=Point(575,10),MinimumSize=Size(100,75),
                  MaximumSize=Size(100,75),Text="Take!")

let clearButton = new Button(Location=Point(575,650),MinimumSize=Size(100,25),
                   MaximumSize=Size(100,25),Text="Clear Game")

                   

(*
let disable bs = 
    for b in [startButton;clearButton;cancelButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false
        *)

// Initialization



window.Controls.Add urlBox
window.Controls.Add loadButton
window.Controls.Add cancelButton

window.Controls.Add takeButton

window.Controls.Add clearButton

window.Controls.Add ansBox

// Start

//Async.StartImmediate (empty())

//Application.Run(window) (* Mac *)
window.Show() (* Windows *)