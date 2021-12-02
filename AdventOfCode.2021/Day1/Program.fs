module Day1

open System.IO

let depthIncrease pair = fst pair < snd pair

let sonarSweep input =
    input
    |> Seq.pairwise
    |> Seq.filter depthIncrease
    |> Seq.length

let sonarSweepGrouped input =
    input
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> sonarSweep

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input" |> Seq.map int
    let part1 = sonarSweep input
    let part2 = sonarSweepGrouped input
    printfn "%A %A" part1 part2
    0
