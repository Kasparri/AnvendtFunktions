// Applied Functional Programming 02257
// Project 1 - Nim Game - Dialogue

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453

open View.fsx

// events

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