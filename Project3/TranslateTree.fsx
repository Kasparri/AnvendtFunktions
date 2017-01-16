
#r @"Machine.dll"
//#load "AST.fs"
//open GuardedCommands.Frontend.AST


open System
open Machine



type Exp =                            
     | N  of int                   (* Integer constant            *)
     | B of bool                   (* Boolean constant            *)
     | Access of Access            (* x    or  ^p    or  a[e]     *)
     | Addr of Access              (* &x   or  &p^   or  &a[e]    *)
     | Apply of string * Exp list  (* Function application        *)

and Access = 
      | AVar of string             (* Variable access        x    *) 
      | AIndex of Access * Exp     (* Array indexing         a[e] *)
      | ADeref of Exp              (* Pointer dereferencing  p^   *)

type Stm  =                            
      | PrintLn of Exp               (* Print                          *) 
      | Ass of Access * Exp          (* x:=e  or  p^:=e  or  a[e]:=e   *)
      | Return of Exp option         (* Return from function           *)   
      | Alt of GuardedCommand        (* Alternative statement          *) 
      | Do of GuardedCommand         (* Repetition statement           *) 
      | Block of Dec list * Stm list (* Block: grouping and scope      *)
      | Call of string * Exp list    (* Procedure call                 *)
           
and GuardedCommand = GC of (Exp * Stm list) list (* Guarded commands    *)

and Dec = 
     | VarDec of Typ * string        (* Variable declaration               *)
     | FunDec of Typ option * string * Dec list * Stm
                                     (* Function and procedure declaration *) 

and Typ  = 
     | ITyp                          (* Type int                    *)
     | BTyp                          (* Type bool                   *)
     | ATyp of Typ * int option      (* Type array                  *)
     | PTyp of Typ                   (* Type pointer                *)
     | FTyp of Typ list * Typ option (* Type function and procedure *)

type Program = P of Dec list * Stm list   (* Program                 *)



type 'a Tree = Node of 'a * ('a Tree list)

let rec treeFromProgram = function
    | P(decs,stms) -> Node ("P", treeFromDecList decs @ treeFromStmList stms)

and treeFromDecList = function
    | x::xs     -> (treeFromDec x) :: (treeFromDecList xs)
    | []        -> []

and treeFromDec = function
    | VarDec(t,s)          -> Node("VarDec",[treeFromType t ; treeFromString s ])
    | FunDec(t,s,decL,stm) -> Node("FunDec",[treeFromTypeOpt t;treeFromString s] @ treeFromDecList decL @ [treeFromStm stm])

and treeFromString (s:string) = Node(s,[])

and treeFromStmList = function
    | x::xs     -> (treeFromStm x)::(treeFromStmList xs)
    | []        -> []

and treeFromStm = function
    | PrintLn(exp)     -> Node("PrintLn",[treeFromExp exp])
    | Ass(acc,exp)     -> Node("Ass",[treeFromAcc acc;treeFromExp exp])
    | Return(expopt)   -> Node("Return",[treeFromExpOpt expopt])
    | Alt(gc)          -> Node("Alt",[treeFromGC gc])
    | Do(gc)           -> Node("Do",[treeFromGC gc])
    | Block(decl,stml) -> Node("Block", (treeFromDecList decl) @ (treeFromStmList stml) )
    | Call(s,expl)     -> Node("Call", (treeFromString s :: treeFromExpList expl) )

and treeFromTypeList = function
    | x::xs     -> (treeFromType x)::(treeFromTypeList xs)
    | []        -> []

and treeFromType = function
    | ITyp            -> Node("ITyp",[])
    | BTyp            -> Node("BTyp",[])
    | ATyp(t, intopt) -> Node("Array",[treeFromType t;treeFromIntOpt intopt])
    | FTyp(tl,topt)   -> Node("Function",(treeFromTypeList tl) @ [treeFromTypeOpt topt])
    | _               -> failwith("We do not support this feature")
    
and treeFromTypeOpt = function
    | None      -> Node("None",[])
    | Some t    -> treeFromType t

and treeFromIntOpt = function
    | None      -> Node("None",[])
    | Some i    -> Node(string i,[])

and treeFromExpList = function
    | x::xs     -> (treeFromExp x) :: (treeFromExpList xs)
    | []        -> []

and treeFromExp = function
    | N(i)          -> Node("N " + string i,[])
    | B(b)          -> Node("B " + string b,[])
    | Access(acc)   -> treeFromAcc acc
    | Apply(s,expl) -> Node("Apply",[treeFromString s;Node("params",(treeFromExpList expl))])
    | _             -> failwith("We do not support this feature")

and treeFromExpOpt = function
    | None      -> Node("None",[])
    | Some t    -> treeFromExp t

and treeFromAcc = function
    | AVar(s)           -> treeFromString s
    | AIndex(acc,exp)   -> Node("Index", [treeFromAcc acc; treeFromExp exp])
    | _                 -> failwith("We do not support this feature")

and treeFromGC = function
    | GC (gc)  -> Node("GC",treeFromExpStmsList gc)

and treeFromExpStmsList = function
    | (exp,stml)::xs -> Node("GC", (treeFromExpStms (exp,stml))) :: (treeFromExpStmsList xs) 
    | []             -> []

and treeFromExpStms = function
    | (exp,stml) -> treeFromExp exp :: treeFromStmList stml