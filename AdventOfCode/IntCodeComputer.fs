module IntCodeComputer

type OpCode = {
  Instruction : int

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
    let opCode = memory.[index]
    
    if opCode = 99 || (opCode <> 1 && opCode <> 2 && opCode <> 3 && opCode <> 4) 
      then ()
    else
      let v1Index = memory.[index+1]
      let v2Index = memory.[index+2]
      let resultIndex = memory.[index+3]
      match opCode with
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

  RunIntCode 0
  memory
