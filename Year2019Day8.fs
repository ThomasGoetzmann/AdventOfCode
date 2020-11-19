module Year2019Day8

open System.IO

//Unpure
let inputs = File.ReadAllText "inputs/input-year2019day8.txt"

//Pure
let Width = 25
let Height = 6
let LayerSize = Width * Height

let toLayers str = str |> List.ofSeq |> List.chunkBySize LayerSize

let occurencesOf char layer = 
    let rec loop counter list =
        match list with
        | [] -> counter
        | head :: tail when (head.Equals(char)) -> loop (counter + 1) tail 
        | _ :: tail -> loop counter tail
    
    loop 0 layer

type Pixel = Transparent | Black | White

let toPixel c =
    match c with
    | '2' -> Transparent
    | '1' -> White
    | '0' -> Black
    | _ -> failwith "Invalid char"

let toPixelLayers str = str |> List.ofSeq |> List.map toPixel |> List.chunkBySize LayerSize

let renderPixels x y =
    match x with
    | Transparent ->  y
    | _ -> x

let rendering layers = 
    let folder x y = 
        match x with
        | [] -> y
        | _ -> List.map2 renderPixels x y

    List.fold folder [] layers


let toConsoleImage render=
    let toString p =
        match p with
        //use 2 characters instead of 1 to make it better printable in the console
        | White -> "██" //white is foreground color, so '██'
        | _ -> "  "     //black or transparent is background color, so '  '

    let toSingleStringWithNewlines strings =
        let lineSize = Width * 2
        let folder (a:string) (b:string) = 
            match a.Length with
            | n when n = lineSize -> a + "\n" + b  //or in F#5 with string interpolation: $"{a}\n{b}", but this isn't much more readable
            | n when n % (lineSize + 1) = 0 -> a + "\n"+ b //lineSize + 1 because of the newline character
            | _ -> a + b
        strings |> List.fold folder ""  

    render |> List.map toString  |> toSingleStringWithNewlines

//Result(unpure)
let layerWithLowestZeros = inputs |> toLayers  |> List.minBy(occurencesOf '0')
let SolveDay8Part1 = (occurencesOf '1' layerWithLowestZeros) * (occurencesOf '2' layerWithLowestZeros)
let SolveDay8Part2 = inputs |> toPixelLayers |> rendering |> toConsoleImage
