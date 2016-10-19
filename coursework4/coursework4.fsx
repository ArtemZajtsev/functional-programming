(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Higher order functions, option, list

  ------------------------------------
  Name: artem zaitsev
  Student ID: arzait
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework4.fsx in directory coursework4.

  The deadline for completing the above procedure is Friday,
  October 21, 2016.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1


let flattenOption v=
    match v with
    | None -> None
    | Some(x) -> x

flattenOption (Some(Some 1))

// 2. Can flattenOption by implemented using bind? If so, do it!
let flatternOption2 v = Option.bind(fun x -> x) v 

flatternOption2 (Some(Some 1))
// 3. Write a function
//
//    idealist : list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes. Pay close attention to the type.

//let idealist (v: list<option<int>>) = List.map (Option.bind(fun x -> if x>0 then Some x else None) ) v
let idealist v = List.choose (fun x -> 
                                                    match x with
                                                    | Some(x) -> Some(x)
                                                    |None -> None
                                                    ) v
idealist [Some 1;None; Some 2; Some 3;None;Some 5]
// 4. Write a function
//
//    conservative : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.

let conservative options = if List.forall(fun (element:option<'a>) -> element.IsSome ) options
                           then Some (idealist options)
                           else None

conservative [Some 1; Some 2; Some 3;Some 5]

// 5. Write a function
//
//    chars : list<string> -> list<char>
//
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']

let chars с= с|>List.collect(fun (x:string) -> List.ofArray (x.ToCharArray()) )
chars ["hello";"world"]
// 6. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"

let iprint range = List.foldBack(fun acc element -> acc.ToString()+","+element.ToString()) range ""
iprint [1..5]