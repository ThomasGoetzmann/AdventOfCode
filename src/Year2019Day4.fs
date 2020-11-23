module Year2019Day4

let toStringList (value: int) = value.ToString() |> Seq.toList

let hasDouble (value: int) = 
  value.ToString() 
  |> Seq.countBy id
  |> Seq.exists (fun (_, count) -> count >= 2) 

let hasPureDouble (value: int) = 
  value.ToString() 
  |> Seq.countBy id
  |> Seq.exists (fun (_, count) -> count = 2) 

let nextAlwaysHigher (value: int) = 
  value.ToString()
  |> Seq.pairwise
  |> Seq.exists (fun (a, b) -> a > b) 
  |> not

let SolveDay4Part1 = 
  [206938..679128] 
  |> Seq.filter hasDouble
  |> Seq.filter nextAlwaysHigher
  |> Seq.length

let SolveDay4Part2 = 
  [206938..679128] 
  |> Seq.filter hasPureDouble
  |> Seq.filter nextAlwaysHigher
  |> Seq.length
