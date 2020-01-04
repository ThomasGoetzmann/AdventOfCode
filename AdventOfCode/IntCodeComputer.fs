module IntCodeComputer

type Operation = 
  | Add
  | Multiply
  | Input
  | Output
  | Halt

type OpCode = {
  Instruction : Operation
  ModeParam1 : int
  ModeParam2 : int
  ModeParam3 : int
}

let parseOperation code = 
  match code with
  | 1 -> Add
  | 2 -> Multiply
  | 3 -> Input
  | 4 -> Output
  | 99 -> Halt
  | _ -> Halt

let parseOpCode (code:int) = {
  Instruction =  code % 100 |> parseOperation; 
  ModeParam1 = code % 1000 / 100; 
  ModeParam2 = code % 10000 / 1000; 
  ModeParam3 = code % 100000 / 10000
  }

let parseIntCode (input:string) = input.Split(",") |> Seq.map int |> Seq.toArray

let InitMemory (verb:int) (noun:int) (memory:int[]) = 
  let mem = Array.copy memory
  mem.[1] <- verb
  mem.[2] <- noun
  mem

let RunIntCode (code:int[]) = 
  let memory = Array.copy code
  
  let rec RunIntCode (index:int) = 
    let opCode = memory.[index] |> parseOpCode
    let v1Index = memory.[index+1]
    let v2Index = memory.[index+2]
    let resultIndex = memory.[index+3]

    match opCode.Instruction with
    | Add -> 
      memory.[resultIndex] <- memory.[v1Index] + memory.[v2Index]
      RunIntCode (index + 4)
    | Multiply -> 
      memory.[resultIndex] <- memory.[v1Index] * memory.[v2Index]
      RunIntCode (index + 4)
    | Input -> 
      ()
      RunIntCode (index + 1)
    | Output -> 
      ()
      RunIntCode (index + 1)
    | Halt -> ()

  let startIndex = 0
  RunIntCode startIndex
  memory
