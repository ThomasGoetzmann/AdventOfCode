module Tests

open System
open FsUnit.Xunit
open Xunit

open Year2019Day1
open Year2019Day2
open Year2019Day3
open Year2019Day4
open Year2019Day5
open Year2019Day6
// day7 todo
open Year2019Day8

[<Fact>]
let ``Day 1 Part 1: Sum of the fuel requirements`` () =
    SolveDay1Part1 |> should equal 3477353

[<Fact>]
let ``Day 1 Part 2: Sum of fuel requirements taking into account the mass of the fuel itself `` () =
    SolveDay1Part2 |> should equal 5213146

[<Fact>]
let ``Day 2 Part 1: Value at 0 when pos1 = 12 and pos2 = 2`` () =
    SolveDay2Part1 |> should equal 4090689

[<Fact>]
let ``Day 2 Part 2: 100 * noun + verb producing the output 19690720`` () =
    SolveDay2Part2 |> should equal 7733

[<Fact>]
let ``Day 3 Part 1: Manhattan distance from the central port to the closest intersection`` () =
    SolveDay3Part1 |> should equal 3247

[<Fact>]
let ``Day 3 Part 2: Fewest combined steps to reach an intersection`` () =
    SolveDay3Part2 |> should equal 48054L

[<Fact>]
let ``Day 4 Part 1: Number of passwords meeting first set of criteria`` () =
    SolveDay4Part1 |> should equal 1653

[<Fact>]
let ``Day 4 Part 2: Number of passwords meeting second set of criteria`` () =
    SolveDay4Part2 |> should equal 1133

[<Fact>]
let ``Day 5 Part 1: Diagnostic code`` () =
    SolveDay5Part1 |> should equal 15508323L

[<Fact>]
let ``Day 5 Part 2: Diagnostic code for system id=5`` () =
    SolveDay5Part2 |> should equal 9006327L

[<Fact>]
let ``Day 6 Part 1: Total direct and indirect orbits`` () =
    SolveDay6Part1 |> should equal 117672

[<Fact>]
let ``Day 6 Part 2: Minimum number of orbital transfers required`` () =
    SolveDay6Part2 |> should equal 277

[<Fact>]
let ``Day 8 Part 1: number of 1 digits * number of 2 digits where fewest 0 digits`` () =
    SolveDay8Part1 |> should equal 1320

[<Fact>]
let ``Day 8 Part 2: ASCII image "RCYKR" (line endings are \r\n)`` () =
    SolveDay8Part2 |> should equal $"{Environment.NewLine}██████      ████    ██      ████    ██  ██████    {Environment.NewLine}██    ██  ██    ██  ██      ████  ██    ██    ██  {Environment.NewLine}██    ██  ██          ██  ██  ████      ██    ██  {Environment.NewLine}██████    ██            ██    ██  ██    ██████    {Environment.NewLine}██  ██    ██    ██      ██    ██  ██    ██  ██    {Environment.NewLine}██    ██    ████        ██    ██    ██  ██    ██  "
    //Looks like this:
    // ██████      ████    ██      ████    ██  ██████
    // ██    ██  ██    ██  ██      ████  ██    ██    ██
    // ██    ██  ██          ██  ██  ████      ██    ██
    // ██████    ██            ██    ██  ██    ██████
    // ██  ██    ██    ██      ██    ██  ██    ██  ██
    // ██    ██    ████        ██    ██    ██  ██    ██ 
