module Year2019Day1

open FSharp.Collections
open System.IO

let inputs = File.ReadLines "inputs/input-year2019day1.txt" |> Seq.map int

let fuel (mass:int) = mass / 3 - 2

let calcFuel (mass:int) = 
    match fuel mass with
    | f when f <= 0 -> 0
    | f -> f

let rec calcFuel2 (mass:int) = 
    match fuel mass with
    | f when f <= 0 -> 0
    | f -> f + calcFuel2 f

let SolveDay1Part1 = inputs |> Seq.sumBy calcFuel
let SolveDay1Part2 = inputs |> Seq.sumBy calcFuel2
