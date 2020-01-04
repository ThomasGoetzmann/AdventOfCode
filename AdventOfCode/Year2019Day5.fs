module Year2019Day5

open System.IO
open IntCodeComputer

let inputs = File.ReadAllText "../../../inputs/input-year2019day5.txt"

let SolveDay5Part1 = 
  Out <- 0L
  inputs |> parseIntCode |> RunIntCode 1 |> ignore
  Out

let SolveDay5Part2 = 
  Out <- 0L
  inputs |> parseIntCode |> RunIntCode 5 |> ignore
  Out