﻿module Year2019Day3

open System.IO

let inputs = File.ReadAllLines "inputs/input-year2019day3.txt"
let testInputs = ["R8,U5,L5,D3"]

type Direction = L | U | R | D
type Move = { Length: int; Direction: Direction}
type Coordinate = {X: int; Y: int}
type Position = {Coordinate: Coordinate; Delay: int64}

let parseDir = 
  function
  | 'L' -> L
  | 'U' -> U
  | 'R' -> R
  | 'D' -> D
  | invalidDirectionChar -> failwithf "Invalid direction: %c" invalidDirectionChar

let parseMove (input: string) = 
  { 
    Length = int input.[1..];
    Direction = parseDir input.[0]
  }

let parse (text: string) = text.Split(",") |> Seq.map parseMove

let drawWireSegment (positions: Position list) (move:Move) = 
  let rec addMove (p: Position) (ps: Position list) (move:Move) =
    let positions = p::ps
    match move with
    | m when m.Length <= 0 -> positions
    | m ->
      let nextMove = {Length = m.Length - 1; Direction = m.Direction}
      let newDelay =  p.Delay + int64 1
      match m.Direction with 
      | L -> addMove {Coordinate={ X = p.Coordinate.X - 1; Y = p.Coordinate.Y}; Delay=newDelay} positions nextMove
      | R -> addMove {Coordinate={ X = p.Coordinate.X + 1; Y = p.Coordinate.Y}; Delay=newDelay} positions nextMove
      | U -> addMove {Coordinate={ X = p.Coordinate.X; Y = p.Coordinate.Y + 1}; Delay=newDelay} positions nextMove
      | D -> addMove {Coordinate={ X = p.Coordinate.X; Y = p.Coordinate.Y - 1}; Delay=newDelay} positions nextMove

  addMove positions.Head positions.Tail move

let drawWire (moves: Move seq) = 
  let rec loop moves positions = 
    match moves with 
    | x::xs -> loop xs (drawWireSegment positions x)
    | [] -> positions
  
  let startPosition = {Coordinate = {X=0; Y=0}; Delay = 0L}
  loop (moves|> Seq.toList) [startPosition]

let Wires = 
  inputs
  |> Seq.map (parse >> drawWire) 

let Intersections wires = 
  wires
  |> Seq.map (fun c -> c |> Seq.map (fun x -> x.Coordinate) |> Seq.toList) 
  |> Seq.map Set.ofList 
  |> Set.intersectMany
  |> Set.filter (fun c -> c <> {X=0; Y=0} )
  |> Set.toSeq

let ManhattanDistance intersections =
  intersections
  |> Seq.map (fun c -> abs c.X + abs c.Y)
  
let SolveDay3Part1 = 
  Wires
  |> Intersections
  |> ManhattanDistance
  |> Seq.min

let filterIntersects (wire: Position list) (wire2: Position list) (intersects: Coordinate seq) = 
  intersects
  |> Seq.map (fun i -> 
  {
      Coordinate = i;
      Delay = (wire |> Seq.find (fun w -> w.Coordinate = i)).Delay + (wire2 |> Seq.find (fun w -> w.Coordinate = i)).Delay
  })

let Delay w = w.Delay

let SolveDay3Part2 = 
  filterIntersects (Wires |> Seq.item 0) (Wires |> Seq.item 1) (Wires |> Intersections) 
  |> Seq.minBy (fun x -> x.Delay)
  |> Delay
