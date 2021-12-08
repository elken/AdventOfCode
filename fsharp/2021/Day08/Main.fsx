[<AutoOpen>]
module Day8

open System.IO
open System

let (<->) (s: string) (p: string) =
    s.Split([| p |], StringSplitOptions.RemoveEmptyEntries)

let parseInput =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Array.map (fun s ->
        match s <-> " | " with
        | [| ins; outs |] -> (ins <-> " ", outs <-> " ")
        | _ -> failwith "Invalid input")

let byPatterns length patterns =
    Array.filter (fun (a: string) -> a.Length = length) patterns
    |> Array.item 0

let intersection list1 list2 =
    Set.intersect (set list1) (set list2) |> Set.toArray

let intersectionCount x y = intersection x y |> Array.length

let outputValue (code: string) patterns =
    let one = byPatterns 2 patterns
    let four = byPatterns 4 patterns
    let seven = byPatterns 3 patterns

    match code.Length with
    | 2 -> "1"
    | 3 -> "7"
    | 4 -> "4"
    | 7 -> "8"
    | 5 ->
        match intersectionCount code one with
        | 2 -> "3"
        | _ ->
            match intersectionCount code four with
            | 3 -> "5"
            | _ -> "2"
    | 6 ->
        match intersectionCount code four with
        | 4 -> "9"
        | _ ->
            match intersectionCount code seven with
            | 2 -> "6"
            | _ -> "0"
    | _ -> failwith "Invalid length"

let calculateLine (ins, outs) =
    outs
    |> Array.map (fun n -> outputValue n ins)
    |> (String.concat "" >> int)

let countUnique (_, outs) =
    outs
    |> Array.filter (fun (s: string) -> Array.contains s.Length [| 2; 3; 4; 7 |])
    |> Array.length

let part1 =
    parseInput |> Array.map countUnique |> Array.sum

let part2 =
    parseInput |> Array.map calculateLine |> Array.sum

printfn "%A %A" part1 part2
