[<AutoOpen>]
module Day1

open System.IO

let hasDepthIncrease pair = fst pair < snd pair

let sonarSweep input =
    input
    |> Seq.pairwise
    |> Seq.filter hasDepthIncrease
    |> Seq.length

let sonarSweepGrouped input =
    input
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> sonarSweep

let input =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Seq.map int

let part1 = sonarSweep input
let part2 = sonarSweepGrouped input
printfn "%A" (part1, part2)
