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
  let rec pathTo object =
    match Map.tryFind object orbits with
    | Some obj -> obj :: pathTo obj
    | None -> []

  pathTo object

let SolveDay6Part1 = 
  let orbits = inputs |> toOrbits
  orbits |> Seq.sumBy (fst >> pathToObject orbits >> List.length)

let SolveDay6Part2 = 
  let orbits = inputs |> toOrbits

  let pathToYOU = pathToObject orbits "YOU" |> List.rev
  let pathToSAN = pathToObject orbits "SAN" |> List.rev

  let distToCommonParent = Seq.zip pathToYOU pathToSAN |> Seq.findIndex (fun (obj1, obj2) -> obj1 <> obj2)

  pathToYOU.Length + pathToSAN.Length - (2 * distToCommonParent)