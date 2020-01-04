module IntCodeComputer

type Operation = 
  | Add
  | Multiply
  | Input
  | Output
  | Halt

type Mode = 
  | Position
  | Immediate

type OpCode = {
  Instruction : Operation
  ModeParam1 : Mode
  ModeParam2 : Mode
  ModeParam3 : Mode
}

let parseOperation code = 
  match code with
  | 1 -> Add
  | 2 -> Multiply
  | 3 -> Input
  | 4 -> Output
  | 99 -> Halt
  | _ -> Halt

let parseMode code = 
  match code with
  | 0 -> Position
  | 1 -> Immediate
  | invalid -> failwithf "Invalid mode: %i" invalid

let parseOpCode (code:int) = {
  Instruction =  code % 100 |> parseOperation; 
  ModeParam1 = code % 1000 / 100 |> parseMode; 
  ModeParam2 = code % 10000 / 1000 |> parseMode; 
  ModeParam3 = Position
  }

let parseIntCode (input:string) = input.Split(",") |> Seq.map int |> Seq.toArray

let mutable Out = 0 

let InitMemory (verb:int) (noun:int) (memory:int[]) = 
  let mem = Array.copy memory
  mem.[1] <- verb
  mem.[2] <- noun
  mem

let RunIntCode (input:int) (code:int[]) = 
  let memory = Array.copy code
  let applyMode index mode = 
    match mode with
    | Position -> memory.[index]
    | Immediate -> index
  
  let rec RunIntCode (index:int) = 
    let opCode = memory.[index] |> parseOpCode
    let v1Index = applyMode (index+1) opCode.ModeParam1
    let v2Index = applyMode (index+2) opCode.ModeParam2
    let resultIndex = applyMode (index+3) opCode.ModeParam3

    match opCode.Instruction with
    | Add -> 
      memory.[resultIndex] <- memory.[v1Index] + memory.[v2Index]
      RunIntCode (index + 4)
    | Multiply -> 
      memory.[resultIndex] <- memory.[v1Index] * memory.[v2Index]
      RunIntCode (index + 4)
    | Input -> 
      memory.[v1Index] <- input
      RunIntCode (index + 2)
    | Output -> 
      Out <- memory.[v1Index]
      RunIntCode (index + 2)
    | Halt -> ()

  let startIndex = 0
  RunIntCode startIndex
  memory
