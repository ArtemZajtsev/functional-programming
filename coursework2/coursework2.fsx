(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists, recursion

  ------------------------------------
  Name: artem zaitsev
  TUT Student ID:arzait
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by September 30, 2016.
*)

// 1. Make a value sl containing empty list of type string list.
let sl = List.empty<List<string>>

// 2. Make a function shuffle: int list -> int list that rearranges the elements of the argument list
// in such a way that the first value goes to first, last to second,
// second to third, last but one to fourth etc.
// E.g.
// shuffle [] -> []
// shuffle [1;2] -> [1;2]
// shuffle [1..4] -> [1;4;2;3]

let rec shuffle (list:List<int>) =
    match list with
    | [] -> []
    | head::tail -> head :: shuffle (List.rev tail)
    //[head;tail.Item(tail.Length-1)] shuffle tail

shuffle []
shuffle [1;2]
shuffle [1..4] 

// 3. Make a function segments: int list -> int list list that splits the list passed
// as an argument into list of lists of nondecreasing segments.
// The segments need to be of maximal possible length (the number of segments
// needs to be minimal)
// E.g.
// segments [] ->  []
// segments [1] -> [[1]]
// segments [3;4;5;5;1;2;3] -> [[3;4;5;5];[1;2;3]]

let rec segments (list:List<int>) =
    match list with
    | [] -> []
    | head::[] -> [[head]]
    | head::tail -> 
                    let rec sorting (a:int list) (b: int list) =
                       if (not b.IsEmpty) && (a |> List.rev |> List.head <= List.head b)
                       then sorting (List.append a [b.Head]) (List.tail b)
                       else a, b     
                    let sort, Res = sorting [head] tail
                    sort :: [(fst (sorting [Res.Head] Res.Tail))]

segments []
segments [1]
segments [3;4;5;5;1;2;3]
(*

let rec segments (list:List<int>) =
    match list with
    | [] -> []
    | head::[] -> [[head]]
    | head::tail -> 
                    let rec sorting (a:int list) (b: int list) =
                       if (not b.IsEmpty) && (a |> List.rev |> List.head <= List.head b)
                       then sorting (List.append a [b.Head]) (List.tail b)
                       else a, b     
                    let sort, Res = sorting [head] tail
                    sort :: [(fst (sorting [Res.Head] Res.Tail))]

*)

(*
let rec sorting (a:int) (b: int list) =
                       if (not b.IsEmpty) && (a <= List.head b)
                       then a ::(sorting (List.head b) (List.tail b))
                       else [a]     
                    sorting head tail
*)

// 4. Make a function sumSublists : int list list -> int list that will compute the sums of sublists in a list of list of ints.
// Hint: use the function List.fold to compute the sums of lists of ints.

let rec sumSublists (list: int list list) =
    match list with
    | [] -> []
    | head :: tail -> List.fold (+) 0 head :: sumSublists  tail

sumSublists [[1;2;3];[1;2];[5;5]]

// 5. Write a function filterSegments : (int list -> bool) -> int list list -> int list list that will filter out lists of ints
// based on some filter function. Write a filter function for even sum, odd sum, even number of elements, odd number of elements.

let rec filterSegments op (list: int list list):int list list =
    match list with 
    | []->[]
    | head::tail -> 
            let filteredTail = filterSegments op tail
            if op head
            then head:: filteredTail
            else filteredTail

let evenSum (list:int list) : bool=
    let pew = (List.fold (+) 0 list) % 2
    match list, pew with
    | [],_ -> false
    |list when pew = 0 -> true 
    |list when pew = 1 -> false
    | _ -> failwith "bad input"

filterSegments (evenSum) [[1;2;3];[1;2];[5;5]]

(*
let oddSum (list:int list) : bool=
    let pew = (List.fold (+) 0 list) % 2
    match list, pew with
    | [],_ -> false 
    |list when pew = 1 -> true
    |list when pew = 0 -> false
    | _ -> failwith "bad input"
*)
let oddSum (list:int list) : bool=
    not (evenSum list)

filterSegments (oddSum) [[1;2;3];[1;2];[5;5]]

let evenNumber (list:int list) : bool=
    let pew = list.Length  % 2
    match list, pew with
    | [],_ -> false 
    |list when pew = 0 -> true
    |list when pew = 1 -> false
    | _ -> failwith "bad input"

filterSegments (evenNumber) [[1;2;3];[1;2];[5;5]]

(*
let oddNumber (list:int list) : bool=
    let pew = list.Length  % 2
    match list, pew with
    | [],_ -> false 
    |list when pew = 1 -> true
    |list when pew = 0 -> false
    | _ -> failwith "bad input"
*)

let oddNumber (list:int list) : bool=
    not (evenNumber list)

filterSegments (oddNumber) [[1;2;3];[1;2];[5;5]]
