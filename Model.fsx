// Applied Functional Programming 02257
// Project 1 - Nim Game - Model

// Mads Ejdesgaard Maibohm s144479
// Kasper Lederballe Sørensen s144453

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 

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
  | Take of (int*int) | Clear | Cancel | Web of string | Error | Cancelled 

let sticks = [5;4;3]

let ev = AsyncEventQueue()

let rec takeaction n h = function
    |x::xs when h = 0 -> x-n::xs
    |x::xs -> takeaction n (h-1) xs
    |[] -> []

let rec ready() = async {
         let! msg = ev.Receive()
         match msg with
            | Clear -> return! ready()
            | Take (n,h)  -> takeaction n h sticks
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
        
         //disable [takeButton;clearButton;loadButton]
         let! msg = ev.Receive()
         match msg with
            | Web html -> printfn "Html %s" html
            | Error -> return! finished("Error")
            | Cancel -> ts.Cancel()
                        return! cancelling()
            | _     -> failwith ("loading:unexpected message")

         }
and cancelling() = 
  async {
         
         //disable [startButton; clearButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")
         }
and player = 
    async {
    
    //disable [loadButton]

    let! msg = ev.Receive()
    match msg with
        |Clear -> return! ready()
        |Take (n,h) -> printf "Take %d %d" n h
        |_ -> failwith("player: unexpected message")
    }
and AI =
    async {
    //disable [loadButton,takeButton,cancelButton]

    let! msg = ev.Receive()
    match msg with
        |Clear -> return! ready()
        |_ -> failwith("AI: unexpected message")
    
    }
and finished(s) =
    async {
    //ansBox.text <- s
    //disable [takeButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Clear -> return! ready()
         | _     ->  failwith("finished: unexpected message")
    }