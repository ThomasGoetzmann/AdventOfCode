module Year2019Day8

open System
open System.IO

let private inputs = File.ReadAllText "inputs/input-year2019day8.txt"

let private width = 25
let private height = 6
let private layerSize = width * height

let private toLayers str = str |> List.ofSeq |> List.chunkBySize layerSize

let private occurencesOf char layer = 
    let rec loop counter list =
        match list with
        | [] -> counter
        | head :: tail when (head.Equals(char)) -> loop (counter + 1) tail 
        | _ :: tail -> loop counter tail
    
    loop 0 layer

type Pixel = Transparent | Black | White

let private toPixel c =
    match c with
    | '2' -> Transparent
    | '1' -> White
    | '0' -> Black
    | _ -> failwith "Invalid char"

let private toPixelLayers str = str |> List.ofSeq |> List.map toPixel |> List.chunkBySize layerSize

let private renderPixels topPixel bottomPixel =
    match topPixel with
    | Transparent ->  bottomPixel
    | _ -> topPixel

let private rendering layers = 
    let folder topLayer bottomLayer = 
        match topLayer with
        | [] -> bottomLayer
        | _ -> List.map2 renderPixels topLayer bottomLayer

    List.fold folder [] layers


let private toConsoleImage render=
    let toString p =
        match p with
        // Use 2 characters instead of 1 to make it better printable in the console
        | White -> "██" // White is foreground color, so '██'
        | _ -> "  "     // Black or transparent is background color, so '  '

    let toSingleStringWithNewlines strings =
        let lineSize = width * 2 + Environment.NewLine.Length - 1
        let folder (a: string) (b: string) = 
            match a.Length with
            | n when n = lineSize -> a + Environment.NewLine + b  // Or in F#5 with string interpolation: $"{a}\n{b}", but this isn't much more readable
            | n when n % (lineSize + 1) = 0 -> a + Environment.NewLine + b // (lineSize + 1) because of the newline character
            | _ -> a + b
        strings |> List.fold folder ""  

    render |> List.map toString  |> toSingleStringWithNewlines

let private layerWithLowestZeros = inputs |> toLayers  |> List.minBy(occurencesOf '0')

let SolveDay8Part1 = (occurencesOf '1' layerWithLowestZeros) * (occurencesOf '2' layerWithLowestZeros)
let SolveDay8Part2 = inputs |> toPixelLayers |> rendering |> toConsoleImage
