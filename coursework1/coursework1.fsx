(*
minhiriath94@gmail.com

  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 1: Basic operations on lists

  ------------------------------------
  Name: artem zaitsev
  TUT Student ID:arzait
  ------------------------------------


  Answer the questions below.  You answers to questions 1--9 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded.

  To submit the coursework you will be asked to

  1) Check out your empty GIT repository
  from the server git.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file courswork1/coursework1.fsx
  in the repository. Commit it and push it to the server!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a grade.


*)
// 1. Make an empty list of generic type.
let genericList = List.empty
// 2. Make an empty list of type 'char list' (or list<char>).
let charList = List.empty<char>
// 3. Make a three element list called 'unis' containing pairs of university
// website url (string) and year of establishment (int). The year of
// establishement should be that of the university.
let tartu = ("http://www.ut.ee/en",1632)
let kpi = ("http://kpi.ua/ru",1898)
let mit = ("http://web.mit.edu/",1861)
let unis = [tartu;kpi;mit]


// 4. Write a function filterOutYoungerThan: int -> string * int -> string * int to filter out news sources
// which are less than some integer years old.  It should use the List.filter
// function from the library.

 //let filterOutYoungerThan = (age :int) (uni: string*int list) : List<string*int>=[]
 // let filterOutYoungerThan = (age :int) (uni: List<string*int>) : List<string*int>


let filterOutYoungerThan age uni=
    let isLessAge x = (2016 - snd x) >= age
    List.filter isLessAge uni
// 5. Test the function 'filterOutYoungerThan' to filter out universities younger than 100 years in
// your list 'unis'.

filterOutYoungerThan 100 unis


// 6. Calculuate the average age of your list of universities. The
// function should use pattern matching and recursion.

let avgAge uni =
    let rec sumList list =
        match list with
        | [] -> 0
        | head :: tail ->  2016-(snd head) + sumList tail

    let sum = sumList uni
    float (sum) / float(uni.Length)


avgAge unis

// 7. Using the http function write a function
//
//    getSource : (string * int) -> (string * string)
//
//    which takes a pair of a url and a year of establishment of the university and
//    returns a pair of a url and the html source of the page.

open System.IO
open System.Net

// get the contents of the url via a web request
let http (url: string) =
  let req = WebRequest.Create(url)
  let resp = req.GetResponse()
  let stream = resp.GetResponseStream()
  let reader = new StreamReader(stream)
  let html = reader.ReadToEnd()
  resp.Close()
  html

let getSource tuple =
    let source = http (fst tuple)
    (fst tuple, source)

getSource tartu

// 8. Write a function
//
//    getSize : (string * string) -> (string * int)
//
//    which takes a pair of a url and its html source and returns a
//    pair of the url and the size of the html (length of the string).
let tartuSource = getSource tartu

let getSize (tuple:string*string) =
    (fst tuple, (snd tuple).Length)

getSize tartuSource


// 9. Write a function
//
//    getSourceSizes : (string * int) list -> (string * int) list
//
//    It should take a list of pairs of urls and years of
//    establishment and return a list of pairs of urls and
//    corresponding html source sizes

let rec getSourceSizes list:List<string*int> =
    match list with
    | [] -> []
    |head::tail -> getSize(getSource head) :: getSourceSizes tail

getSourceSizes unis
