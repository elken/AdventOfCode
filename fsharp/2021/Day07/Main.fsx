[<AutoOpen>]
module Day7

open System.IO

let triangularNumber n = n * (n + 1) / 2

let calculateDistance mapper = (Seq.map mapper >> Seq.sum)

let input =
    File.ReadAllText $"{__SOURCE_DIRECTORY__}/input"
    |> fun s -> s.Split(",") |> Seq.map int |> Seq.sort

let median = Seq.item (Seq.length input / 2) input

let mean =
    Seq.average (input |> Seq.map float) |> int

let part1 =
    input
    |> calculateDistance (fun n -> abs (n - median))

let part2 =
    input
    |> calculateDistance (fun n -> triangularNumber (abs (n - mean)))

printfn "%A" (part1, part2)
