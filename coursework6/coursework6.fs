(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Property based testing

  ------------------------------------------------
  Name:artem zaitsev
  Student ID:arzait
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as
  file coursework6.fsx in directory coursework6.

  The file that should be compiled to a dll should go into coursework6.fs.

  Please do not upload DLL-s. Just include a readme.txt file containing the 
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 11, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

module coursework6


open NUnit.Framework
open FsCheck.NUnit
open FsCheck

#if INTERACTIVE
#r @"..\packages\NUnit.2.6.4\lib\nunit.framework.dll"
#r @"D:\f #\test\packages\FsCheck.2.6.2\lib\net45\FsCheck.dll"
#endif


let rec isPalindrome xs =
    match xs with
    | []        -> true
    | (x :: xs) -> match List.rev xs with
                        | []        -> true
                        | (y :: ys) -> x = y && isPalindrome ys

let toPalindrome xs =
    let len       = List.length xs
    let suffixLen = len / 2
    let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
    let take n xs = Seq.toList (Seq.take n xs)
    take prefixLen xs @ List.rev (take suffixLen xs)

let palindromeGenerator list =
    gen {
        return toPalindrome list
    }

[<TestFixture>]
type ``coursework6`` () =
(*
    Task 1:

    Define FsCheck properties for the following statements:

      * Taking the lengths of two lists xs and ys and adding them yields the
        same value as concatenating xs and ys and taking the length of the
        result.

      * Reversing two lists xs and ys and concatenating the results yields the
        same value as concatenating ys and xs and reversing the result.
*) 
    [<Property>]
        member this.``length of two lists equals length of concatenated list`` (xs: int list) (ys: int list) = (xs.Length + ys.Length) = List.length (xs @ ys)
    [<Property>]
        member this.``reverse and concat equals to concatenate and reverse`` (xs: int list) (ys: int list) = List.rev (xs) @ List.rev(ys) = List.rev(ys @ xs)
(*
    Task 2:

    A palindrome is a list that is equal to its reverse. Below you find a
    function isPalindrome, which checks whether a given list is a palindrome.

     a) Define an FsCheck property that expresses the above definition of a
        palindrome. Use the operator ==> for defining your property.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)
    [<Property(EndSize=5)>]
        member this.task2A (listOfPalindromes:list<int>) = isPalindrome listOfPalindromes ==> (listOfPalindromes = List.rev listOfPalindromes)
    [<Property(EndSize=5)>]
        member this.task2B (listOfPalindromes:list<int>) = isPalindrome listOfPalindromes ==> (listOfPalindromes = List.rev listOfPalindromes) |> Prop.collect (List.length listOfPalindromes)
(*
    Task 3:

    Below you find a function toPalindrome, which converts a given list into a
    palindrome of the same length, more or less by replacing the second half of
    the list with the reverse of the first half.

     a) Define an FsCheck property that expresses the definition of a
        palindrome from the previous task. This time, make sure that FsCheck
        does not generate arbitrary lists from which it uses only the
        palindromes, but generates palindromes directly.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)
    [<Property>]
        member this.task3a (list: int list) =Prop.forAll(Arb.fromGen(palindromeGenerator list)) (fun pew -> pew = List.rev pew)
    [<Property(Verbose = true)>]
        member this.task3b (list: int list) = Prop.forAll(Arb.fromGen(palindromeGenerator list)) (fun pew -> pew = List.rev pew) |> Prop.collect (List.length list)

(*  Task 5:

    Take the decision tree code from the lecture notes and write unit and property based tests
    to the extent that you consider the code reasonably well tested.

*)


type Client = 
    { Name : string;
      Income : int ;
      YearsInJob : int
      UsesCreditCard : bool;
      CriminalRecord : bool }

type QueryInfo =
    { Title     : string
      Check     : Client -> bool
      Positive  : Decision
      Negative  : Decision }

and Decision = 
    | Result of string
    | Query  of QueryInfo

let rec tree =
    Query  {Title = "More than €40k"
            Check = (fun cl -> cl.Income > 40000)
            Positive = moreThan40
            Negative = lessThan40}
and moreThan40 =
    Query  {Title = "Has criminal record"
            Check = (fun cl -> cl.CriminalRecord)
            Positive = Result "NO"
            Negative = Result "YES"}
and lessThan40 =
    Query  {Title = "Years in job"
            Check = (fun cl -> cl.YearsInJob > 1)
            Positive = Result "YES"
            Negative = usesCreditCard}
and usesCreditCard =
    Query  {Title = "Uses credit card"
            Check = (fun cl -> cl.UsesCreditCard)
            Positive = Result "YES"
            Negative = Result "NO"}

let rec testClientTree client tree =
    match tree with
    | Result msg  -> msg
    | Query qinfo ->    let result, case = 
                            if qinfo.Check(client) then
                                "yes", qinfo.Positive
                            else
                                "no", qinfo.Negative
                        printfn " - %s ? %s" qinfo.Title result
                        testClientTree client case

    

let john = {Name = "John Doe";
            Income = 40000 ;
            YearsInJob = 1 ; 
            UsesCreditCard = true ;
            CriminalRecord = false }
    
    
testClientTree john tree

let personGenerator = 
    gen {
        let! income = Gen.choose(0,100000)
        let! yearsInJob = Gen.choose(0,10)
        let! useCreditCard = Arb.generate<bool>
        let! criminalRecord = Arb.generate<bool>
        return {
                    Name = "John";
                    Income = income;
                    YearsInJob = yearsInJob;
                    UsesCreditCard = useCreditCard;
                    CriminalRecord = criminalRecord
                }
    }

Gen.sample 5 5 personGenerator

[<TestFixture>]
type ``Task 5`` () =
    let richAndCriminal = {Name = "John"; Income = 60000; YearsInJob = 1; UsesCreditCard = true; CriminalRecord = true }
    let richAndNotCriminal = {Name = "John"; Income = 60000; YearsInJob = 1; UsesCreditCard = true; CriminalRecord = false}
    let poorAndManyExperience = {Name = "John"; Income = 500; YearsInJob = 2; UsesCreditCard = true; CriminalRecord = false}
    let poorLowExperienceHasCreditCard ={Name = "John"; Income = 500; YearsInJob = 0; UsesCreditCard = true; CriminalRecord = false}
    let poorLowExperienceNoCreditCard ={Name = "John"; Income = 500; YearsInJob = 0; UsesCreditCard = false; CriminalRecord = false}
    [<Test>]
    member this.``criminal and rich - NO`` ()=
        Assert.AreEqual(testClientTree richAndCriminal tree,"NO")
    [<Test>]
    member this.``not a criminal and rich - YES`` ()=
        Assert.AreEqual(testClientTree richAndNotCriminal  tree,"YES")
    [<Test>]
    member this.``poor and has Experience - YES`` ()=
        Assert.AreEqual(testClientTree poorAndManyExperience  tree,"YES")
    [<Test>]
    member this.``poor and low Experience but has credit card - YES`` ()=
        Assert.AreEqual(testClientTree poorLowExperienceHasCreditCard  tree,"YES")
    [<Test>]
    member this.``poor and low Experience and no credit card - NO`` ()=
        Assert.AreEqual(testClientTree poorLowExperienceNoCreditCard  tree,"NO")