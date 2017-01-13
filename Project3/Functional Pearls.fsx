/// Functional Pearls - Drawing Trees
/// Kasper L. Sørensen & Mads E. Maibohm
/// s144453              s144479


/// basic auxilliary functions
let mean (x,y) = (x+y)/2.0



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
    | (_,_)                 -> 0.0

// Fitting a list of extents from the left
let fitlistl (es:Extent list) =
    let rec fitlistl' acc = function
    | []      -> []
    | (e::es) -> let x = - (fit (acc,e)) 
                 x :: (fitlistl' (merge ( acc , moveextent(e,x) ) ) es)
    fitlistl' [] es
// Fitting a list of extents from the right
let fitlistr (es:Extent list) =
    let rec fitlistr' acc = function
    | []      -> []
    | (e::es) -> let x = fit (acc,e)
                 x :: (fitlistr' (merge ( moveextent(e,x), acc ) ) es)
    fitlistr' [] (List.rev es)

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