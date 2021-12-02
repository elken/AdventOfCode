module Day2

open System.IO

type Depth =
    { HorizontalPosition: int
      VerticalPosition: int
      Aim: int }

let tailToAmount (tail: string list) = tail.[0] |> int

let calculateDepth depth direction =
    match direction with
    | "forward" :: amt -> { depth with HorizontalPosition = depth.HorizontalPosition + (amt |> tailToAmount) }
    | "down" :: amt -> { depth with VerticalPosition = depth.VerticalPosition + (amt |> tailToAmount) }
    | "up" :: amt -> { depth with VerticalPosition = depth.VerticalPosition - (amt |> tailToAmount) }
    | _ -> depth

let dive input =
    input
    |> Seq.fold
        calculateDepth
        { HorizontalPosition = 0
          VerticalPosition = 0
          Aim = 0 }
    |> (fun depth -> depth.HorizontalPosition * depth.VerticalPosition)

let calculateAim depth direction =
    match direction with
    | "forward" :: amt ->
        { depth with
            HorizontalPosition = depth.HorizontalPosition + (amt |> tailToAmount)
            VerticalPosition =
                depth.VerticalPosition
                + depth.Aim * (amt |> tailToAmount) }
    | "down" :: amt -> { depth with Aim = depth.Aim + (amt |> tailToAmount) }
    | "up" :: amt -> { depth with Aim = depth.Aim - (amt |> tailToAmount) }
    | _ -> depth

let aim input =
    input
    |> Seq.fold
        calculateAim
        { HorizontalPosition = 0
          VerticalPosition = 0
          Aim = 0 }
    |> (fun depth -> depth.HorizontalPosition * depth.VerticalPosition)

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "input"
        |> Seq.map (fun s -> List.ofArray (s.Split ' '))

    let part1 = dive input
    let part2 = aim input
    printfn "%A %A" part1 part2
    0
