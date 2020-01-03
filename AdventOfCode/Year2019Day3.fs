module Year2019Day3

open System.IO

let inputs = File.ReadAllLines "../../../inputs/input-year2019day3.txt"
let testInputs = ["R8,U5,L5,D3"]

type Direction = L | U | R | D
type Move = { Length:int; Direction:Direction}
type Coordinate = {X:int; Y:int}

let parseDir = 
  function
  | 'L' -> L
  | 'U' -> U
  | 'R' -> R
  | 'D' -> D
  | invalidDirectionChar -> failwithf "Invalid direction: %c" invalidDirectionChar

let parseMove (input:string) = 
  { 
    Length = int input.[1..];
    Direction = parseDir input.[0]
  }

let parse (text:string) = text.Split(",") |> Seq.map parseMove

let drawWireSegment (coordinates:List<Coordinate>, move:Move) = 
  let rec addMove (coordinates:List<Coordinate>, position:Coordinate, move:Move) =
    match move with
    | m when m.Length <= 0 -> coordinates
    | m -> 
      match m.Direction with 
      | L -> 
        let newCoordinate = { X = position.X - 1; Y = position.Y }
        addMove (newCoordinate::coordinates, newCoordinate, {Length = m.Length - 1; Direction = m.Direction})
      | R -> 
        let newCoordinate = { X = position.X + 1; Y = position.Y }
        addMove (newCoordinate::coordinates, newCoordinate, {Length = m.Length - 1; Direction = m.Direction})
      | U -> 
        let newCoordinate = { X = position.X; Y = position.Y + 1 }  
        addMove (newCoordinate::coordinates, newCoordinate, {Length = m.Length - 1; Direction = m.Direction})
      | D -> 
        let newCoordinate = { X = position.X; Y = position.Y - 1 }
        addMove (newCoordinate::coordinates, newCoordinate, {Length = m.Length - 1; Direction = m.Direction})
  addMove (coordinates, coordinates.Head, move)

let drawWire (moves:seq<Move>) = 
  let mutable coordinates = [ {X=0; Y=0} ]
  for move in moves do
    coordinates <- drawWireSegment(coordinates,move)
  coordinates

let ManhattanDistance coordinate = abs coordinate.X + abs coordinate.Y

let SolveDay3Part1 = 
  inputs 
  |> Seq.map parse 
  |> Seq.map drawWire
  |> Seq.map Set.ofList
  |> Set.intersectMany
  |> Set.map ManhattanDistance
  |> Set.toSeq
  |> Seq.filter (fun x -> x <> 0) //removes the starting point which is also considered as a intersection point
  |> Seq.min
  
