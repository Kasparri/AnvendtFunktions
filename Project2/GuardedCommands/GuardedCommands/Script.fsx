﻿// Michael R. Hansen 05-01-2016

// You must revise 4 pathes occurring in this file 
// The first three are:

#r @"bin/Debug/FSharp.PowerPack.dll";;
#r @"bin/Debug/Machine.dll";
#r @"bin/Debug/VirtualMachine.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "TypeCheck.fs"
#load "CodeGen.fs"
#load "CodeGenOpt.fs"
#load "Util.fs"
#load "FunctionalPearls.fs"
//#load "DumbFunctionalPearls.fs"
#load "TranslateTree.fs"

#time "on"



open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.FunctionalPearls
//open GuardedCommands.Frontend.DumbFunctionalPearls
open GuardedCommands.Frontend.TranslateTree
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

open Machine
open VirtualMachine

// You must revise this path
//System.IO.Directory.SetCurrentDirectory @"../GuardedCommands";;
//System.IO.Directory.SetCurrentDirectory @" ../Users/Kasper/Workspace/AnvendtFunktions/Project2/GuardedCommands/GuardedCommands";;
System.IO.Directory.SetCurrentDirectory @"C:\Users\Mads_\OneDrive\Documents\Visual Studio 2015\Projects\AnvendtFunktions\Project2\GuardedCommands\GuardedCommands";;

// The Ex0.gc example:
let rec randomTree = function
        | 0 -> Node("Leaf",[])
        | d -> Node("A",[randomTree(d-1);randomTree(d-1)])

let compileToFile prog name = let parsed = parseFromFile prog
                              let tree = treeFromProgram parsed
                              let design = design tree
                              createFile (converttree design) name;;



let simpleTree = Node("A",[Node("B",[]);Node("C",[])]);;
let desSimple = design simpleTree;;
let _ = createFile (converttree desSimple) "Simple";;


let ex0Prog = parseFromFile "Ex1.gc";;
let ex0Tree = treeFromProgram ex0Prog;;
let ex0Design = design ex0Tree;;
let _ = createFile (converttree ex0Design) "Ex1";;

let factProg = parseFromFile "fact.gc";;
let factTree = treeFromProgram factProg;;
let factDesign = design factTree;;
let _ = createFile (converttree factDesign) "fact";;

let qsProg = parseFromFile "QuickSortV1.gc";;
let qsTree = treeFromProgram qsProg;;
let qsDesign = design qsTree;;
let _ = createFile (converttree qsDesign) "QuickSort";;



//let _ = tcP ex0Tree;;
//
//let ex0Code = CP ex0Tree;; 
//
//let _ = go ex0Tree;;
//
//let _ = goTrace ex0Tree;;


// Parsing of Ex1.gc

let ex1Tree = parseFromFile "Ex1.gc";; 

// -- is typechecked as follows:

let _ = tcP ex1Tree;;

// obtain symbolic code:
let ex1Code = CP ex1Tree;; 

// -- is executed with trace as follows:
let stack = goTrace ex1Tree;;

// -- is executed as follows (no trace):
let sameStack = go ex1Tree;;

// "All in one" parse from file, type check, compile and run 

let _ = exec "Ex1.gc";;

let _ = exec "Ex2.gc";;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
//List.iter execOpt ["Ex1.gc"; "Ex2.gc"];;

// All programs relating to the basic version can be parsed:
let pts = List.map parseFromFile ["Ex1.gc"; "Ex2.gc";"Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc"];;

// The parse tree for Ex3.gc, changed to use List.item
List.item 2 pts;


// Test of programs covered by the first task (Section 3.7):
List.iter exec ["Ex1.gc"; "Ex2.gc";"Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc"];;

// Test of programs covered by the second task (Section 4.3):
List.iter exec ["Ex7.gc"; "fact.gc"; "factRec.gc"; "factCBV.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
List.iter exec ["A0.gc"; "A1.gc"; "A2.gc"; "A3.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
List.iter exec ["A4.gc"; "Swap.gc"; "QuickSortV1.gc"];;
(*
// Test of programs covered by the fifth task (Section 7.4):
List.iter exec ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;
*)
