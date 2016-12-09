(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 9: Asynchronous and reactive programming

  ------------------------------------------------------------------------------
  Name:artem zaitsev
  Student ID:arzait
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework9.fsx in directory coursework9.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 9, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

open System.IO
open System.Net
(*
  Task 1:

  Write a function downloadParallel : string list -> Async<string []> that takes
  a list of URLs and downloads the resources referenced by these URLs in
  parallel. Use the function downloadAsync from the lecture in your
  implementation.
*)

let readToEndAsync (reader : StreamReader) =
  Async.AwaitTask (reader.ReadToEndAsync())


let downloadAsync (url : string) =
  async {
    let  request  = HttpWebRequest.Create(url)
    use! response = request.AsyncGetResponse()
    let  stream   = response.GetResponseStream()
    use  reader   = new StreamReader(stream)
    return! readToEndAsync reader
  }

let uniList = ["http://ttu.ee/";"http://www.ut.ee/en";"http://kpi.ua/en"; "http://www.ut.ee/en/admissions"]

let downloadParallel list =
    let asyncList = List.map downloadAsync list
    Async.Parallel asyncList

Async.RunSynchronously (downloadParallel uniList)

(*
  Task 2:

  Write a function downloadSemiParallel : string list -> Async<string []> that
  takes a list of URLs and downloads the resources referenced by these URLs.
  Resources from URLs with the same domain name shall be downloaded
  sequentially, but otherwise, parallelism shall be used. The order of the
  resources in the resulting array can be chosen by you.
*)
open System

let downloadSync (url : string) =
  let request  = HttpWebRequest.Create(url)
  use response = request.GetResponse()
  let stream   = response.GetResponseStream()
  use reader   = new StreamReader(stream)
  reader.ReadToEnd()

let getHost uriString =
    let uri = System.Uri(uriString)
    uri.Host

let groupByDomain list =
    list
    |> Seq.groupBy (fun x ->getHost x)
    |> Seq.map(fun x -> snd x |> Seq.toList)
    |> Seq.toList
        
groupByDomain uniList



let downloadSemiParallel list =
    async {
         let grouped = groupByDomain list
         let asyncStuff = grouped|> List.map(fun(urls)-> urls|> List.map(fun url-> downloadSync url))
         return asyncStuff |>List.concat |>List.toArray
    }

downloadSemiParallel uniList
(*
  Task 3:

  Write an event stream additions : IObservable<string> that emits an event
  everytime a file is created in the current directory. Each such event shall
  carry the name of the created file.

  Furthermore, write an event stream removals : IObservable<string> that emits
  an event everytime a file is removed from the current directory. Each such
  event shall carry the name of the removed file.
*)

open System.IO

let watcher = new FileSystemWatcher(__SOURCE_DIRECTORY__)
watcher.EnableRaisingEvents <- true

let additions = watcher.Created |> Observable.map(fun eventArgs -> eventArgs.Name)

let reactAdditionStream () = additions |> Observable.add (printfn "created %s")

reactAdditionStream () 

let removals = watcher.Deleted |> Observable.map(fun eventArgs -> eventArgs.Name)

let reactRemovalStream () = removals |> Observable.add (printfn "deleted %s")

reactRemovalStream () 
(*
  Task 4:

  Below you find the definition of a type Change whose values represent changes
  to a directory. Use the event streams from Task 3 to define an event stream
  changes : IObservable<Change> of all file additions and removals in the
  current directory.
*)

type Change =
  | Addition of string
  | Removal  of string

let additionMsg = additions |> Observable.map(fun x -> Addition x)

let removeMsg = removals |> Observable.map (fun x ->Removal x)

let changes = Observable.merge additionMsg removeMsg

let reactMsgs () = changes |> Observable.add (fun x -> match x with 
                                                       | Addition s -> printfn "created %s " s
                                                       | Removal s -> printfn "deleted %s " s)

reactMsgs ()

(*
  Task 5:

  Use the event stream changes from Task 4 to define an event stream
  turnover : IObservable<int> that tells at every file addition or removal how
  much the number of files in this directory has increased since the beginning
  (with negative numbers signifying a decrease). For example, if two files are
  added and one file is removed afterwards, there should be three events, that
  carry the numbers 1, 2, and 1, respectively.
*)

let turnover = changes |> Observable.scan (fun count x -> match x with
                                                          | Addition s-> count+1
                                                          | Removal s -> count-1 ) 0

let reactCounts () = turnover |> Observable.add (printfn "%d")

reactCounts ()