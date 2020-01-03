module IntCodeComputer

type OpCode = {
  Instruction : int
  ModeParam1 : int
  ModeParam2 : int
  ModeParam3 : int
}

let parseIntCode (input:string) = input.Split(",") |> Seq.map int |> Seq.toArray

let InitMemory (verb:int) (noun:int) (memory:int[]) = 
  let mem = Array.copy memory
  mem.[1] <- verb
  mem.[2] <- noun
  mem

let ReadOpCode (code:int) = {
  Instruction = code % 100; 
  ModeParam1 = code % 1000 / 100; 
  ModeParam2 = code % 10000 / 1000; 
  ModeParam3 = code % 100000 / 10000
  }

let RunIntCode (code:int[]) = 
  let memory = Array.copy code
  
  let rec RunIntCode (index:int) = 
    let opCode = memory.[index] |> ReadOpCode
    let instruction = opCode.Instruction
    
    if instruction = 99 || (instruction <> 1 && instruction <> 2 && instruction <> 3 && instruction <> 4) 
      then ()
    else
      let v1Index = memory.[index+1]
      let v2Index = memory.[index+2]
      let resultIndex = memory.[index+3]
      match instruction with
      | 1 -> 
        memory.[resultIndex] <- memory.[v1Index] + memory.[v2Index]
        RunIntCode (index + 4)
      | 2 -> 
        memory.[resultIndex] <- memory.[v1Index] * memory.[v2Index]
        RunIntCode (index + 4)
      | 3 -> 
        ()
        RunIntCode (index + 1)
      | 4 -> 
        ()
        RunIntCode (index + 1)
      | _ -> ()

  let startIndex = 0
  RunIntCode startIndex
  memory
