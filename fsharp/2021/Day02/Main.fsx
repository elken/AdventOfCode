type SubmarinePosition =
    { HorizontalPosition: int
      Depth: int
      Aim: int }

let (|Forward|Down|Up|) (line: string) =
    match line.Split(" ") with
    | [| command; amount |] ->
        if command = "forward" then
            Forward(amount |> int)
        elif command = "down" then
            Down(amount |> int)
        else
            Up(amount |> int)
    | _ -> failwith "Invalid line"

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"

let applyInputs calculate input =
    input
    |> Seq.fold
        calculate
        { HorizontalPosition = 0
          Depth = 0
          Aim = 0 }
    |> (fun position -> position.HorizontalPosition * position.Depth)

let calculateDepth position command =
    match command with
    | Forward amount -> { position with HorizontalPosition = position.HorizontalPosition + amount }
    | Down amount -> { position with Depth = position.Depth + amount }
    | Up amount -> { position with Depth = position.Depth - amount }

let calculateAim position command =
    match command with
    | Forward amount ->
        { position with
            HorizontalPosition = position.HorizontalPosition + amount
            Depth = position.Depth + position.Aim * amount }
    | Down amount -> { position with Aim = position.Aim + amount }
    | Up amount -> { position with Aim = position.Aim - amount }

let part1 = applyInputs calculateDepth input
let part2 = applyInputs calculateAim input

printfn "%A" (part1, part2)
