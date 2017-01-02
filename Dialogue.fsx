// Applied Functional Programming 02257
// Project 1 - Nim Game - Dialogue

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453


// The dialogue automaton 
let ev = AsyncEventQueue()
let rec ready() = 
  async {urlBox.Text <- "http://"
         ansBox.Text <- ""

         disable [cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Start url -> return! loading(url)
         | Clear     -> return! ready()
         | _         -> failwith("ready: unexpected message")}
  
and loading(url) =
  async {ansBox.Text <- "Downloading"
         use ts = new CancellationTokenSource()

          // start the load
         Async.StartWithContinuations
             (async { let webCl = new WebClient()
                      let! html = webCl.AsyncDownloadString(Uri url)
                      return html },
              (fun html -> ev.Post (Web html)),
              (fun _ -> ev.Post Error),
              (fun _ -> ev.Post Cancelled),
              ts.Token)

         disable [startButton; clearButton]   
         let! msg = ev.Receive()
         match msg with
         | Web html ->
             let ans = "Length = " + String.Format("{0:D}",html.Length)
             return! finished(ans)
         | Error   -> return! finished("Error")
         | Cancel  -> ts.Cancel()
                      return! cancelling()
         | _       -> failwith("loading: unexpected message")}

and cancelling() =
  async {ansBox.Text <- "Cancelling"
         
         disable [startButton; clearButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}

and finished(s) =
  async {ansBox.Text <- s
         
         disable [startButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Clear -> return! ready()
         | _     ->  failwith("finished: unexpected message")}