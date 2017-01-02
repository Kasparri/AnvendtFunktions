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
let loadButton = new Button(Location=Point(415,10),MinimumSize=Size(50,25),
                 MaximumSize=Size(50,25),Text="Load Game")


let takeButton = new Button(Location=Point(575,10),MinimumSize=Size(100,75),
                 MaximumSize=Size(100,75),Text="Take!")

let clearButton = new Button(Location=Point(575,650),MinimumSize=Size(100,25),
                  MaximumSize=Size(100,25),Text="Clear Game")




// Initialization



window.Controls.Add urlBox
window.Controls.Add loadButton

window.Controls.Add takeButton

window.Controls.Add clearButton

// Start

//Async.StartImmediate (empty())

Application.Run(window) (* Mac *)
//window.Show() (* Windows *)