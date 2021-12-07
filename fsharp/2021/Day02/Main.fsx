[<AutoOpen>]
module Day2

open System.IO

type SubmarineInput =
    | Forward of int
    | Down of int
    | Up of int

type SubmarinePosition =
    { HorizontalPosition: int
      Depth: int
      Aim: int }

let parseDirection (line: string) =
    let tokens = line.Split(" ")

    let command =
        match tokens.[0] with
        | "forward" -> Some Forward
        | "down" -> Some Down
        | "up" -> Some Up
        | _ -> None

    command.Value(tokens.[1] |> int)

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

let applyInputs calculate input =
    input
    |> Seq.fold
        calculate
        { HorizontalPosition = 0
          Depth = 0
          Aim = 0 }
    |> (fun position -> position.HorizontalPosition * position.Depth)

let dive = applyInputs calculateDepth

let aim = applyInputs calculateAim

let input =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Seq.map parseDirection

let part1 = dive input
let part2 = aim input
printfn "%A" (part1, part2)
