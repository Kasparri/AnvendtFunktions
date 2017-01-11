namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck = 

/// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
/// for global and local variables 
   let rec tcE gtenv ltenv = function                            
         | N _              -> ITyp   
         | B _              -> BTyp   
         | Access acc       -> tcA gtenv ltenv acc     
                   
         | Apply(f,[e]) when List.exists (fun x ->  x=f) ["-"; "!"]  
                            -> tcMonadic gtenv ltenv f e        

         | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+";"*";"-"; "="; "&&"]        
                            -> tcDyadic gtenv ltenv f e1 e2   
         | Apply(f,es) -> tcNaryFunction gtenv ltenv f es

         | _                -> failwith "tcE: not supported yet"

   and tcMonadic gtenv ltenv f e = match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | _           -> failwith "illegal/illtyped monadic expression" 
   
   and tcDyadic gtenv ltenv f e1 e2 = match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-"]  -> ITyp
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["="] -> BTyp
                                      | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"="]     -> BTyp 
                                      | _                      -> failwith("illegal/illtyped dyadic expression: " + f)

   and tcNaryFunction gtenv ltenv f es = match Map.tryFind f gtenv with
                                         | None -> failwith "Function not declared"
                                         | Some (FTyp (typs,topt)) when es.Length = typs.Length -> for i in 0..(typs.Length-1) do
                                                                                                        if (typs.[i] <> tcE gtenv ltenv es.[i]) 
                                                                                                        then Console.WriteLine typs.[i]
                                                                                                             Console.WriteLine (tcE gtenv ltenv es.[i])
                                                                                                             failwith "Parameters are of the wrong types"
                                                                                                   match topt with
                                                                                                    | None -> failwith "Procedures are not functions"
                                                                                                    | Some t -> t 
                                                                                                   
                                         | Some (FTyp (typs,topt)) -> failwith "Wrong number of parameters"
                                         | _ -> failwith "Unexpected function, expected a function"
 
   and tcNaryProcedure gtenv ltenv f es = failwith "type check: procedures not supported yet"
      

/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables 
   and tcA gtenv ltenv = 
         function 
         | AVar x         -> match Map.tryFind x ltenv with
                             | None   -> match Map.tryFind x gtenv with
                                         | None   -> failwith ("no declaration for : " + x)
                                         | Some t -> t
                             | Some t -> t            
         | AIndex(acc, e) -> match tcA gtenv ltenv acc with
                             | ATyp(atyp,topt) -> if (ITyp = tcE gtenv ltenv e) then atyp
                                                  else failwith "Index was not an integer"
                             | _ -> failwith"Variable was not an array"
                             
         | ADeref e       -> failwith "tcA: pointer dereferencing not supported yes"
 

/// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
   and tcS gtenv ltenv = function                           
                         | PrintLn e -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e) -> if tcA gtenv ltenv acc = tcE gtenv ltenv e 
                                         then ()
                                         else failwith "illtyped assignment"                                

                         | Block([],stms) -> List.iter (tcS gtenv ltenv) stms

                         | Block(decs,stms) -> List.iter (tcS gtenv (tcLDecs ltenv decs)) stms

                         | Alt gc  -> tcGC gtenv ltenv gc
                         | Do (GC []) -> ()
                         | Do  gc  -> tcGC gtenv ltenv gc

                         | Return expopt -> if (Map.exists (fun _ typ -> match typ with
                                                                          | FTyp (_,topt) when topt.IsNone -> expopt.IsNone
                                                                          | FTyp (_,topt) -> tcE gtenv ltenv expopt.Value = topt.Value
                                                                          | _ -> false ) ltenv) then ()
                                            else failwith "No function to return from"
                                     
                         | _              -> failwith "tcS: this statement is not supported yet"

   and tcGDec gtenv ltenv = function  
                      | VarDec(t,s)               -> match t with
                                                     | ATyp (atyp, topt) -> match atyp with
                                                                            | ITyp | BTyp ->  Map.add s t gtenv
                                                                            | _ -> failwith "Cant declare arrays of anything but ints and bools"
                                                     | _ -> Map.add s t gtenv
                      | FunDec(topt,f, decs, stm) -> let typeList = decsToTypes decs []
                                                     let newGtenv = tcGDecs (Map.add f (FTyp (typeList, topt)) gtenv) ltenv decs
                                                     ignore(tcS newGtenv (Map.add f (FTyp (typeList, topt)) ltenv)  stm)
                                                     newGtenv

   and tcGDecs gtenv ltenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv ltenv dec) ltenv decs
                       | _         -> gtenv

   and tcLDec ltenv = function  
                      | VarDec(t,s)               -> Map.add s t ltenv
                      | _ -> failwith "Local function declarations are not allowed"

   and tcLDecs ltenv = function
                       | dec::decs -> tcLDecs (tcLDec ltenv dec) decs
                       | _         -> ltenv

   and decsToTypes decs seen = match decs with
                               | [] -> []
                               | VarDec(t,s)::decs' when List.exists (fun v -> v=s) seen -> failwith "Two params were equal"
                               | VarDec(t,s)::decs' -> t::(decsToTypes decs' (s::seen))
                               | FunDec(topt,f,decs,stm)::decs' when List.exists (fun v -> v=f) seen -> failwith "Two params were equal"
                               | FunDec(topt,f,decs,stm)::decs' -> match topt with
                                                                   |None -> failwith"A void function was used as a statement"
                                                                   |Some t -> t::(decsToTypes decs' (f::seen))


/// tcGS gtenv ltenv gc checks the well-typeness of a guarded command on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
   and tcGC gtenv ltenv = function
       | GC ((e, stms)::[])  ->  if tcE gtenv ltenv e = BTyp
                                 then List.iter (tcS gtenv ltenv) stms
                                 else failwith "ill typed condition"
       | GC ((e, stms)::rest) -> if tcE gtenv ltenv e = BTyp
                                 then List.iter (tcS gtenv ltenv) stms
                                      tcGC gtenv ltenv (GC rest)
                                 else failwith "ill typed condition" 
       | GC []               ->  failwith "illtyped guarded command" 

/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty Map.empty decs
                            List.iter (tcS gtenv Map.empty) stms