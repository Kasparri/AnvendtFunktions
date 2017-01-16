/// Functional Pearls - Drawing Trees
/// Kasper L. Sørensen & Mads E. Maibohm
/// s144453              s144479

open System
open System.IO
open System.Diagnostics


/// 2. Representing trees
type 'a Tree = Node of 'a * ('a Tree list)

// Horizontal movement of a tree
let movetree (Node((label, x),subtrees) , (x':float) ) = Node((label, x+x'), subtrees)



/// 3. Representing extents
type Extent = (float*float) list

// Horizontal movement of an extent, e by a distance, x
let moveextent ((e:Extent),x) = List.map (fun (p,q) -> (p+x,q+x)) e

// Merging two non-overlapping extents 
let rec merge (e1:Extent,e2:Extent) =
    match (e1,e2) with
    | ([],qs)               -> qs
    | (ps,[])               -> ps
    | ((p,_)::ps, (_,q)::qs) -> (p,q) :: merge (ps,qs)
let mergelist (es:Extent list) = List.fold (fun v e -> merge (v,e)) [] es



/// 4. Fitting extents
// Returns the minimal distance between the roots of two extents
let rec fit (e1:Extent,e2:Extent) =
    match (e1,e2) with
    | ((_,p)::ps,(q,_)::qs) -> max (fit (ps,qs)) (p - q + 1.0)
    | _                     -> 0.0



// Fitting a list of extents from the left
let fitlistl (es:Extent list) =
    let rec fitlistl' acc = function
    | []      -> []
    | (e::es) -> let x = (fit (acc,e)) 
                 x :: (fitlistl' (merge ( acc , moveextent(e,x) ) ) es)
    fitlistl' [] es
// Fitting a list of extents from the right
let fitlistr (es:Extent list) =
    let rec fitlistr' acc = function
    | []      -> []
    | (e::es) -> let x = - fit(e,acc)
                 x :: (fitlistr' (merge ( moveextent(e,x), acc ) ) es)
    List.rev (fitlistr' [] (List.rev es))

let mean (x,y) = (x+y)/2.0

// Obtain a symmetric layout
let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es))



/// 5. Designing the tree
let rec design' (Node(label, subtrees)) =
    let (trees, extents) = List.unzip (List.map design' subtrees)
    let positions        = fitlist extents
    let ptress           = List.map movetree (List.zip trees positions)
    let pextents         = List.map moveextent (List.zip extents positions )
    let resultextent     = (0.0,0.0) :: mergelist pextents
    let resulttree       = Node((label, 0.0), ptress)
    (resulttree,resultextent)

let design tree = fst(design' tree)


/// Convert Trees to strings of PostScript format

let moveto x y = (string (x*100.0)) + " " + (string y) + " moveto\n"

let lineto x y = (string (x*100.0)) + " " + (string y) + " lineto\n"

let checkName n = if (String.length n) > 15 then n.[0..14] else n

let drawName px d n i = moveto (px + i) (-d*80-10) +
                        "(" + (checkName n) + ") dup stringwidth pop 2 div neg 0 rmoveto show\n"

let drawDown x ystart yend = moveto x ystart + lineto x yend

let drawHorizontalBar xstart xend y = moveto xstart y + lineto xend y

let extracti = function
               | Node((_,i),_) -> i

let drawLayer x d kids = drawDown x (-d*80 - 20) (-d*80 - 40) +
                         drawHorizontalBar (extracti (List.head kids) + x) (extracti (List.last kids) + x) (-d*80 - 40) +
                         (List.fold (fun r (Node ((_,i),_)) -> r + (drawDown (i+x) (-d*80-40) (-d*80-80))) "" kids)
                                            


let rec converttree' px d = function
    | Node ((n, i),[]  )  -> drawName (px) d n i
    | Node ((n, i),kids)  -> drawName (px) d n i + drawLayer (px+i) d kids +
                             List.fold (fun r k -> r + converttree' (px+i) (d+1) k) "" kids



let converttree tree = 
    let start = "%!\n" +
                "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n" +
                "1 1 scale\n" +
                "700 999 translate\n" +
                "newpath\n" +
                "/Times-Roman findfont 10 scalefont setfont\n"

    let finish = "stroke\n" + "showpage\n"
    start + (converttree' 0.0 0 tree) + finish 

    (*
let randomtree d = Node("Start",randomtree' [] d)

let rec randomtree' acc = function
    | 0 -> [Node("Leaf",[])]
    | d -> let rand = System.Random()
           let n = rand.Next(1,5)
           let mutable kids = []
           for i in 1..n do
                 kids <- kids @ Node(string i, randomtree' [] (d-1))
           [Node("A",kids)]
           //acc @ randomtree Node("A", (randomtree [] (d-1)) ) (d-1)


           *)

/// Writing PostScript to a file
let createFile (data:string) fileName =
    use streamWriter = new StreamWriter("Users/Kasper/Workspace/AnvendtFunktions/Project3/" + fileName + ".ps", false)
    streamWriter.Write(data)


/// Tests
let tree1 = Node("A", [Node("B",[Node("D",[]);Node("E",[])]);Node("C",[Node("F",[]);Node("G",[])]);Node("H",[])] )
let tree2 = Node("A",[])

design tree1;;
design tree2;;

let postree1 = design tree1;;
let postree2 = design tree2;;

converttree postree1;;
converttree postree2;;

createFile (converttree postree1) "test123";;




(*
// Used for extent testing
let ext1 = [(1.0,1.0);(2.0,2.0)]
let ext2 = [(0.0,22.32)]
let ext3 = []
let extL = [ext1;ext2;ext3]
*)



