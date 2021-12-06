module Day6

open System.IO

let calculateTimer idx count newFish =
    match idx with
    | 6 -> count + newFish
    | 8 -> newFish
    | _ -> count

let calculateAges ages =
    match ages with
    | [] -> ages
    | newFish :: agesTail ->
        (agesTail
         |> List.mapi (fun idx count -> calculateTimer idx count newFish))
        @ [ newFish ]

let lanternAges days input =
    let rec loop daysLeft ages =
        match daysLeft with
        | 0 -> ages
        | _ -> loop (daysLeft - 1) (calculateAges ages)

    loop days input |> List.sum

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllText "input"
        |> (fun s -> s.Split(",") |> Seq.map int)
        |> Seq.countBy id
        |> Seq.sortBy fst
        |> Seq.map (snd >> int64)

    let firstGeneration =
        List.replicate 9 0
        |> List.mapi (fun idx elem ->
            match Seq.tryItem (idx - 1) input with
            | Some count -> (int64 elem) + count
            | _ -> (int64 elem))

    let part1 = lanternAges 18 firstGeneration
    let part2 = lanternAges 256 firstGeneration
    printfn "%A %A" part1 part2
    0
