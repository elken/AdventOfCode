[<AutoOpen>]
module Day3

open System
open System.IO

let binaryToInt binary = Convert.ToInt32(binary, 2)

let mode seq =
    seq |> Seq.countBy id |> Seq.maxBy snd |> fst

let applyConverter converter input =
    input
    |> Seq.transpose
    |> Seq.map (mode >> converter)
    |> String.concat ""
    |> binaryToInt

let gamma input = applyConverter string input

let epsilon input =
    applyConverter (fun s -> if s = '0' then "1" else "0") input

let binaryDiagnostic input = gamma input * epsilon input

let mostCommonValue input idx sorter =
    input
    |> Seq.map (Seq.item idx)
    |> Seq.countBy id
    |> sorter
    |> fst

let bitFilter sorter input =
    let rec loop idx bits =
        let filter (bit: string) =
            bit.[idx] = (mostCommonValue bits idx sorter)

        match Seq.length bits with
        | 1 -> Seq.head bits
        | _ -> loop (idx + 1) (Seq.filter filter bits |> Seq.toList)

    loop 0 input

let o input =
    input
    |> bitFilter (Seq.sortDescending >> Seq.maxBy snd)
    |> binaryToInt

let co2 input =
    input
    |> bitFilter (Seq.sort >> Seq.minBy snd)
    |> binaryToInt

let lifeSupport input = o input * co2 input

let input =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Seq.toList

let part1 = binaryDiagnostic input
let part2 = lifeSupport input
printfn "%A" (part1, part2)
