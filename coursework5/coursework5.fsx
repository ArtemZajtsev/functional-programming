(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Working with data: intro to type providers and charting

  ------------------------------------
  Name: artem zaitsev
  TUT Student ID: arzait
  ------------------------------------


  Answer the questions below.  You answers to questions 1--6 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 29, 2016.
*)

// 1) The following three different fuel consumption units for vehicles are in use:
//    * litres per 100 km
//    * miles per imperial gallon (in use in the UK)
//    * miles per US gallon (in use in the US)
// 1.a) Define the units in terms of units of measure.
[<Measure>] type km //Kilometer
[<Measure>] type l //Liter
[<Measure>] type m //mile
[<Measure>] type ig //Imperial gallon
[<Measure>] type g // US gallon
// 1.b) Define 2 functions that convert the appropriate US and imperial mpg values to
//      litres per 100 km. 
let ConvertUSMPGtoLPK (mpg : float<m/g>) = 1.0<m/g>/mpg*235.215<l/km>
let ConvertIMPGtoLPK (mpg: float<m/ig>)= 1.0<m/ig>/mpg*282.48176271<l/km>

let mpg = 100.0<m/g>
let impg = 100.8<m/ig>
ConvertUSMPGtoLPK  mpg
ConvertIMPGtoLPK impg
// 1.c) Define a function that converts litres per 100 km of appropriate fuel to
//      CO2 emissions g per km.
[<Measure>] type gr //grams 
let lpk = 10.0<l/km>
let dieselEmission = 26.5<gr/l>
let gasolineEmission = 23.2<gr/l>
let autogasEmission = 19.0<gr/l>

let Convert (lp100km:float<l/km>) (standartEmissionForLP100KM:float<gr/l>) = lp100km*standartEmissionForLP100KM
//diesel
Convert lpk dieselEmission
//gasoline 
Convert lpk gasolineEmission
//autogas
Convert lpk autogasEmission 

// 2) Get the fuel consumption data
// 2.a) in imperial MPG (miles per imperial gallon) of at least 20 vehicles from
// http://carfueldata.direct.gov.uk/search-by-fuel-economy.aspx
// Save the data in file called imperial.csv

//done

// 2.b) Get the fuel consumption data of at least 20 cars in US MPG (miles per US gallon) from
// https://www.fueleconomy.gov/feg/download.shtml
// save the data in file called us.csv

//done

// 3) Load the imperial.csv and us.csv files using FSharp.Data.CsvProvider<>

#r @"packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll" //TODO: Change directory route when submit!!

open FSharp.Data

//let imperial = CsvFile.Load("D:/TartuUniversity/F#/arzait/coursework5/imperial.csv").Cache() //TODO:Change directory route when submit!!
//let us = CsvFile.Load("D:/TartuUniversity/F#/arzait/coursework5/us.csv").Cache() //TODO: Change directory route when submit!!
type imperialProvider = CsvProvider<"imperial.csv"> 
let imperialData = imperialProvider.Load("imperial.csv")
//let imperial = imperialProvider.Load("imperial.csv")
type usProvider = CsvProvider<"us.csv">
let usData = usProvider.Load("us.csv")
//type usProvider = CsvProvider<"us.csv">
//let us = usProvider.Load("us.csv") 

// 4) Write a function to convert the appropriate mpg data into
//    litres per 100 km using the functions defined in Q1.

let convertImp = [for row in imperialData.Rows -> row.ImperialCombined] |> List.ofSeq |> List.map(fun element ->  ConvertIMPGtoLPK (((float)element*1.0<m/ig>)))
convertImp

let convertUs = [for row in usData.Rows -> row.``City MPG``] |> List.ofSeq |> List.map(fun element ->  ConvertUSMPGtoLPK (((float)element*1.0<m/g>)))
convertUs
// 5) Display the converted data in an appropriate chart (select the type that is most 
//    appropriate for displaying the data).
#load @"packages\FSharp.Charting.0.90.14\FSharp.Charting.fsx"

open FSharp.Charting
open FSharp.Charting.ChartTypes
//let inperialChart = convertImp |> Chart.FastLine
//let usChat = convertUs |> Chart.FastLine
let imperialCarModels = [for row in imperialData.Rows -> row.Model] |> List.ofSeq
let usCarModels = [for row in usData.Rows -> row.Model] |> List.ofSeq
let imperialChart = Chart.Line(convertImp,Name="Liters per 100 km from Imperial galons",Title="L/100km",Labels=imperialCarModels ,Color=System.Drawing.Color.PaleVioletRed,XTitle="Car Number",YTitle="L/100km")
let usChart = Chart.Line(convertUs,Name="Liters per 100 km from US galons",Title="L/100km",Labels=usCarModels,Color=System.Drawing.Color.Aquamarine,XTitle="Car Number",YTitle="L/100km")
 

// 6) Combine the data from 2 data sources into a single chart. Add appropriate titles and
//    legends. 

Chart.Combine(
        [imperialChart
         usChart]) |> Chart.WithLegend(InsideArea = false, Docking = Docking.Top) |> Chart.WithYAxis(true,"L/100km") |> Chart.WithXAxis(true,"Car Number",Min=0.0,ToolTip = "Probably not the best name for axis")
