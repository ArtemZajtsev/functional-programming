(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: User defined types

  ------------------------------------
  Name:
  TUT Student ID:
  ------------------------------------


  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 9, 2015.
*)

// 1. Consider expression trees of type ExprTree declared in the lecture.
// Extend the type with if-then-else expression of the form:
// if b then e1 else e2
// where b is a boolean expression and e1 and e2 are expressions.
// An example expression could be:
// if a+3 > b+c && a>0 then c+d else e
type ExprTree = | Const of int
                | Ident of string
                | Minus of ExprTree
                | Sum of ExprTree * ExprTree
                | Diff of ExprTree * ExprTree
                | Prod of ExprTree * ExprTree
                | Let of string * ExprTree * ExprTree
                | If of ExprTree * ExprTree * ExprTree 
                | Moar of ExprTree * ExprTree
                | And of ExprTree * ExprTree
                
                

// 2. Extend the function eval defined in the lecture to support the
// if-then-else expressions defined in Q1.

let decider n  =
    match n with
    | 1 -> true
    | 0 -> false
    | _ -> failwith "bad input"


let rec eval t env = 
    match t with
    | Const n -> n
    | Ident s -> Map.find s env
    | Minus t -> -(eval t env)
    | Sum (t1,t2) -> eval t1 env + eval t2 env
    | Diff(t1,t2) -> eval t1 env - eval t2 env
    | Prod(t1,t2) -> eval t1 env * eval t2 env
    | Let (s,t1,t2) -> let v1 = eval t1 env
                       let env1 = Map.add s v1 env
                       eval t2 env1
    | If (c,t,e) -> if (decider (eval c env)) = true
                    then eval t env
                    else eval e env
    | Moar (a,b) -> if eval a env> eval b env
                    then 1
                    else 0
    | And (a,b) -> if (eval a env) = 1 && (eval b env) = 1
                   then 1
                   else 0

// if a+3 > b+c && a>0 then c+d else e
let envGood = Map.ofList ["a",-2;"b",1;"c",3;"d",4;"e",5]
let expr1 = If(And(Moar(Sum(Ident "a", Const 3),Sum(Ident "b",Ident "c")),(Moar(Ident "a", Const 0))),Sum(Ident "c",Ident "d"),Ident "e")
eval expr1 envGood

// 3-4: Given the type definition:
// type BList =
//  | BEmpty
//  | Snoc of BList * int
//
// 3. Make the function filterB: (prop: int -> bool) BList -> BList that will return a list for the elements of which
// the function prop returns true.
type BList = | BEmpty
             | Snoc of BList * int

let list1 = Snoc(Snoc(Snoc(Snoc(BEmpty,0),3),2),1)

let rec filterB (prop: int -> bool) (list: BList) : BList =
    match list with
    | BEmpty -> BEmpty
    | Snoc(head,tail) ->
            let filteredHead = filterB prop head
            if prop tail 
            then Snoc(filteredHead,tail)
            else filteredHead

filterB (fun x -> x>=2) list1
             
// 4. Make the function mapB: (trans: int -> int) BList -> BList that will return a list where the function trans has
// been applied to each element.

let rec mapB (trans: int -> int) (list:BList) : BList =
    match list with
    | BEmpty -> BEmpty
    | Snoc(head,tail) -> Snoc (mapB trans head,trans tail)

let list2 = Snoc(Snoc(Snoc(Snoc(BEmpty,0),3),2),1)

mapB (fun x -> x+1) list2
// 5-7. Given the type definition
// type Tree =
//  | Nil
//  | Branch2 of Tree * int * Tree
//  | Branch3 of Tree * int * Tree * int * Tree
//
// 5. Define the value exampleTree : Tree that represents the following
//    tree:
//
//        2
//       / \
//      *  3 5
//        / | \
//       *  *  *
type Tree =
  | Nil
  | Branch2 of Tree * int * Tree
  | Branch3 of Tree * int * Tree * int * Tree

let exampleTree = Branch2(Nil,2,Branch3(Nil,3,Nil,5,Nil) )

// 6. Define a function sumTree : Tree -> int that computes the sum of
//    all labels in the given tree.
let rec sumTree (tree:Tree) : int =
    match tree with 
    | Nil -> 0
    | Branch2(branch1,treeHead,branch2) -> treeHead + sumTree branch1 + sumTree branch2
    | Branch3(branch1,head1,branch2,head2,branch3) -> head1 + head2 + sumTree branch1 + sumTree branch2 + sumTree branch3

sumTree exampleTree
// 7. Define a function productTree : Tree -> int that computes the
//    product of all labels in the given tree. If this function
//    encounters a label 0, it shall not look at any further labels, but
//    return 0 right away.

let exampleTreeWitn0 = Branch2(Nil,2,Branch3(Nil,0,Nil,5,Nil) )

let rec productTree (tree:Tree) : int =
    match tree with
    | Nil -> 1
    | Branch2(branch1,0,branch2) -> 0
    | Branch3(branch1,0,branch2,_,branch3) -> 0
    | Branch3(branch1,_,branch2,0,branch3) -> 0
    | Branch2(branch1,head1,branch2) -> head1 * productTree branch1 * productTree branch2
    | Branch3(branch1,head1,branch2,head2,branch3) -> head1 * head2 * productTree branch1 * productTree branch2 * productTree branch3


productTree exampleTree
productTree exampleTreeWitn0 
// ** Bonus questions **

// 8. Extend the ExprTree type with a pattern match expression
// match p with [p1, ex1; p2,ex2 ...]

type ExprTree8 = | Const8 of int
                 | Ident8 of string
                 | Minus8 of ExprTree8
                 | Sum8 of ExprTree8 * ExprTree8
                 | Diff8 of ExprTree8 * ExprTree8
                 | Prod8 of ExprTree8 * ExprTree8
                 | Let8 of string * ExprTree8 * ExprTree8
                 | Match8 of ExprTree8*List<(ExprTree8*ExprTree8)>

// 9. Extend the eval function to support match expressions.
let rec eval8 (t:ExprTree8) env = 
    match t with
    | Const8 n -> n
    | Ident8 s -> Map.find s env
    | Minus8 t -> -(eval8 t env)
    | Sum8 (t1,t2) -> eval8 t1 env + eval8 t2 env
    | Diff8(t1,t2) -> eval8 t1 env - eval8 t2 env
    | Prod8(t1,t2) -> eval8 t1 env * eval8 t2 env
    | Let8 (s,t1,t2) -> let v1 = eval8 t1 env
                        let env1 = Map.add s v1 env
                        eval8 t2 env1
    | Match8(p,list) -> let result =  snd(list |>(List.find (fun (x,y) -> x = p )))
                        eval8 result env

                        //| _ -> eval8 (List.head((List.find (fun x -> if  not x.Empty then x) result)) env
                        //eval8 (List.head((List.find (fun x -> if  not x.Empty then x) result)) env 

let expr8 = Match8(Const8 5, [(Const8 1, Const8 100); (Const8 5, Const8 999)])
eval8 expr8 Map.empty
