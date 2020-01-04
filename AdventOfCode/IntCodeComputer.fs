module IntCodeComputer

type Operation = 
  | Add
  | Multiply
  | Input
  | Output
  | JumpTrue
  | JumpFalse
  | Less
  | Equals
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
  | 5 -> JumpTrue
  | 6 -> JumpFalse
  | 7 -> Less
  | 8 -> Equals
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

let mutable Out = 0L

let InitMemory (verb:int) (noun:int) (memory:int[]) = 
  let mem = Array.copy memory
  mem.[1] <- verb
  mem.[2] <- noun
  mem

let RunIntCode (input:int) (code:int[]) = 
  let memory = Array.copy code
  let applyMode index mode = 
    if (index + 1 >= memory.Length) then 0
    else 
      match mode with
      | Position -> memory.[index]
      | Immediate -> index
  
  let rec RunIntCode (index:int) = 
    let opCode = memory.[index] |> parseOpCode
    //TODO : depending on the operation, not all params are actually required, fix this and remove the "if..else" in "applyMode"
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
      Out <- int64 memory.[v1Index]
      RunIntCode (index + 2)
    | JumpTrue ->
      match memory.[v1Index] with
      | 0 -> RunIntCode (index + 3)
      | _ -> RunIntCode memory.[v2Index] 
    | JumpFalse ->
      match memory.[v1Index] with
      | 0 -> RunIntCode memory.[v2Index]
      | _ -> RunIntCode (index + 3)
    | Less ->
      match (memory.[v1Index], memory.[v2Index]) with
      | (v1,v2) when v1 < v2 -> memory.[resultIndex] <- 1
      | _ -> memory.[resultIndex] <- 0
      RunIntCode (index + 4)
    | Equals ->
      match (memory.[v1Index], memory.[v2Index]) with
      | (v1,v2) when v1 = v2 -> memory.[resultIndex] <- 1
      | _ -> memory.[resultIndex] <- 0
      RunIntCode (index + 4)
    | Halt -> ()

  let startIndex = 0
  RunIntCode startIndex
  memory
