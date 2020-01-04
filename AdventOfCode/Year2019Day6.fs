module Year2019Day6

open System.IO

let inputs = File.ReadAllLines "../../../inputs/input-year2019day6.txt"

let toOrbit (line:string) =
  let object = line.Split(')')
  object.[1], object.[0]

let toOrbits lines =
  lines |> Seq.map toOrbit

let pathToObject orbits object=
  let orbits = (orbits |> Map.ofSeq)
  let rec pathTo object counter =
    match Map.tryFind object orbits with
    | Some obj -> pathTo obj (counter + 1)
    | None -> counter

  pathTo object 0 

let SolveDay6Part1 = 
  let orbits = inputs |> toOrbits
  orbits |> Seq.sumBy (fst >> pathToObject orbits)

let SolveDay6Part2 = 0