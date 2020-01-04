module Year2019Day5

open System.IO
open IntCodeComputer

let inputs = File.ReadAllText "../../../inputs/input-year2019day5.txt"

let SolveDay5Part1 = 
  inputs |> parseIntCode |> RunIntCode 1 |> ignore
  Out
