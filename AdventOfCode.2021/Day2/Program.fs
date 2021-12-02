module Day2

open System.IO

type SubmarineInput =
    | Forward of int
    | Down of int
    | Up of int

type SubmarinePosition =
    { HorizontalPosition: int
      VerticalPosition: int
      Aim: int }

let initialState =
    { HorizontalPosition = 0
      VerticalPosition = 0
      Aim = 0 }

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
    | Down amount -> { position with VerticalPosition = position.VerticalPosition + amount }
    | Up amount -> { position with VerticalPosition = position.VerticalPosition - amount }

let calculateAim position command =
    match command with
    | Forward amount ->
        { position with
            HorizontalPosition = position.HorizontalPosition + amount
            VerticalPosition = position.VerticalPosition + position.Aim * amount }
    | Down amount -> { position with Aim = position.Aim + amount }
    | Up amount -> { position with Aim = position.Aim - amount }

let applyInputs calculate input =
    input
    |> Seq.fold calculate initialState
    |> (fun depth -> depth.HorizontalPosition * depth.VerticalPosition)

let dive = applyInputs calculateDepth

let aim = applyInputs calculateAim

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "input"
        |> Seq.map parseDirection

    let part1 = dive input
    let part2 = aim input
    printfn "%A %A" part1 part2
    0
