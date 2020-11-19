module Year2019Day2

open System.IO
open IntCodeComputer

let inputs = File.ReadAllText "inputs/input-year2019day2.txt"
let testInputs = "1,0,0,3,99"

let Solve (verb:int) (noun:int) = inputs |> parseIntCode |> InitMemory verb noun |> RunIntCode 1 |> Seq.head

let SolveDay2Part1 = Solve 12 2
let SolveDay2Part2 = seq {
  for noun = 0 to 99 do
      for verb = 0 to 99 do
          if Solve noun verb = 19690720 then
              100 * noun + verb } |> Seq.head
