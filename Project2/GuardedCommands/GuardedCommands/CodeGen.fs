namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)
   type Var = 
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables *)

   type varEnv = Map<string, Var*Typ> * int

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

      (* Adapted from MicroC *)
   let bindParam (env, fdepth) (VarDec(typ, x))  : varEnv = 
       (Map.add x (LocVar fdepth, typ) env , fdepth+1)
   let bindParams (paras:Dec list) ((env, fdepth) : varEnv) : varEnv = 
       List.fold bindParam (env, fdepth) paras;


/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
   let rec CE vEnv fEnv = 
       function
       | N n          -> [CSTI n]
       | B b          -> [CSTI (if b then 1 else 0)]
       | Access acc   -> CA vEnv fEnv acc @ [LDI] 

       | Apply("-", [e]) -> CE vEnv fEnv e @  [CSTI 0; SWAP; SUB]
       | Apply("!", [e]) -> CE vEnv fEnv e @  [NOT]

       | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                let labfalse = newLabel()
                                CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labfalse; CSTI 0; Label labend]

       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "*"; "-"; "="]
                             -> let ins = match o with
                                          | "+"  -> [ADD]
                                          | "*"  -> [MUL]
                                          | "-"  -> [SUB]
                                          | "="  -> [EQ] 
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins 

       | Apply(f, es) -> callfun f es vEnv fEnv

       | _            -> failwith "CE: not supported yet"
       

/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
   and CA vEnv fEnv = function | AVar x         -> match Map.find x (fst vEnv) with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> [GETBP; CSTI addr; ADD]
                               | AIndex(acc, e) -> failwith "CA: array indexing not supported yet" 
                               | ADeref e       -> failwith "CA: pointer dereferencing not supported yet"

   (* Adapted from MicroC *)
   and callfun f es vEnv fEnv : instr list =
    let (labf, tyOpt, paramdecs) = Map.find f fEnv
    let argc = List.length es
    if argc = List.length paramdecs then
      CEs vEnv fEnv es @ [CALL(argc, labf)]
    else
      failwith "parameter/argument mismatch"


   and CEs vEnv fEnv es = List.collect (CE vEnv fEnv) es 

  
(* Bind declared variable in env and generate code to allocate it: *)   
   let allocate (kind : int -> Var) (typ, x) (vEnv : varEnv)  =
    let (env, fdepth) = vEnv 
    match typ with
    | ATyp (ATyp _, _) -> 
      raise (Failure "allocate: array of arrays not permitted")
    | ATyp (t, Some i) -> failwith "allocate: array not supported yet"
    | _ -> 
      let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+1)
      let code = [INCSP 1]
      (newEnv, code)

      (*
   let makeLocalEnvs decs vEnv fEnv =
       let rec addv decs vEnv fEnv =
           match decs with
           | [] -> (vEnv, fEnv, [] )
           | dec::decr ->
               match dec with
               | VarDec (typ, var) -> let (vEnv1, code1) = allocate LocVar (typ, var) vEnv
                                      let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                                      (vEnv2, fEnv2, code1 @ code2)
               | FunDec _ -> failwith "Local function declarations not allowed"
       addv decs vEnv fEnv *)

   let rec loop decs vEnv =
       match decs with 
       | []     -> (snd vEnv, [])
       | VarDec (typ,x)::decs' -> 
            let (vEnv1, code1) = allocate LocVar (typ, x) vEnv
            let (fdepthr, coder) = loop decs' vEnv1 
            (fdepthr, code1 @ coder)
       | _ -> failwith "gg"

                       
/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment                          
   let rec CS vEnv fEnv = function
       | PrintLn e        -> CE vEnv fEnv e @ [PRINTI; INCSP -1] 

       | Ass(acc,e)       -> CA vEnv fEnv acc @ CE vEnv fEnv e @ [STI; INCSP -1]

       //| Block([],stms)   -> CSs vEnv fEnv stms
       | Block(decs,stms) -> let rec loop decs vEnv =
                                 match decs with 
                                 | []     -> (snd vEnv, [])
                                 | VarDec (typ,x)::decs' -> 
                                      let (vEnv1, code1) = allocate LocVar (typ, x) vEnv
                                      let (fdepthr, coder) = loop decs' vEnv1 
                                      (fdepthr, code1 @ coder)
                                 | _ -> failwith "gg"
                             let (fdepthend, code) = loop decs vEnv
                             code @ CSs vEnv fEnv stms @ [INCSP(snd vEnv - fdepthend)]

       | Alt gc -> let labend = newLabel()
                   let labnext = newLabel()
                   let labStop = newLabel()
                   match gc with
                   | GC ((e,stms)::[]  ) -> CE vEnv fEnv e @ [IFZERO labStop] @ CSs vEnv fEnv stms @ [GOTO labend;
                                              Label labStop; STOP]
                   | GC ((e,stms)::rest) -> CE vEnv fEnv e @ [IFZERO labnext] @ CSs vEnv fEnv stms @ [GOTO labend;
                                              Label labnext] @ CS vEnv fEnv (Alt (GC rest))
                   | _ -> [STOP]
                   @ [Label labend]
       | Do gc ->  let labstart = newLabel()
                   let labend = newLabel()
                   let labnext = newLabel()
                   [Label labstart] @
                   match gc with
                   | GC ((e,stms)::[]  ) -> CE vEnv fEnv e @ [IFZERO labend] @ CSs vEnv fEnv stms @ [GOTO labstart]
                   | GC ((e,stms)::rest) -> CE vEnv fEnv e @ [IFZERO labnext] @ CSs vEnv fEnv stms @ [GOTO labstart; 
                                            Label labnext] @ CS vEnv fEnv (Do (GC rest))
                   | _ -> []
                   @ [Label labend]

       //| Return None -> [RET (snd vEnv - 1)] (* MicroC inspired *)

       | Return (Some e) -> CE vEnv fEnv e @ [RET (snd vEnv)] (* MicroC inspired *)

       | _                -> failwith "CS: this statement is not supported yet"


   and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms 



(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)




   let makeGlobalEnvs decs = 
       let rec addv decs vEnv fEnv = 
           match decs with 
           | []         -> (vEnv, fEnv, [])
           | dec::decr  -> 
             match dec with
             | VarDec (typ, var) -> let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                                    (vEnv2, fEnv2, code1 @ code2)
             | FunDec (tyOpt, f, xs, body) -> addv decr vEnv (Map.add f (newLabel(), tyOpt, xs) fEnv) (* Adapted from MicroC *)
       addv decs (Map.empty, 0) Map.empty





/// CP prog gives the code for a program prog
   let CP (P(decs,stms)) = 
       let _ = resetLabels ()
       let ((gvM,_) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs

       (* Adapted from MicroC *)
       let compilefun (tyOpt, f, xs, stm) =
           let (labf,_,paras) = Map.find f fEnv
           let (envf, fdepthf) = bindParams paras (gvM, 0)
           let code = CS (envf,fdepthf) fEnv stm
           [Label labf] @ code @ [RET (List.length paras-1)]
       let functions =
           List.choose (function
                           | FunDec (rTy, name, argTys, stm)
                                   -> Some (compilefun (rTy, name, argTys, stm))
                           | VarDec _ -> None)
                        decs
        
       initCode @ CSs gvEnv fEnv stms 
       @ [STOP]
       @ List.concat functions