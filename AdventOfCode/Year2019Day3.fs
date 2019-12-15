module Year2019Day3

open System.IO

let inputs = File.ReadAllLines "../../../inputs/input-year2019day3.txt"

let parseToArray (text:string) = text.Split(",") |> Seq.toArray

let coordinates1 = inputs.[0] |> parseToArray
let coordinates2 = inputs.[1] |> parseToArray


