[<AutoOpen>]
module Day10

open System.IO

let pairs =
    Map.ofList [ ('(', ')')
                 ('[', ']')
                 ('{', '}')
                 ('<', '>') ]

let isOpening delim = Map.containsKey delim pairs

let isClosing delim =
    Map.exists (fun _ value -> value = delim) pairs

let parseInput =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"

let getOffenders withStack line =
    let rec loop stack chars =
        match stack, chars with
        | _, next :: tail when isOpening next -> loop (next :: stack) tail
        | _, next :: tail when isClosing next ->
            match stack with
            | openChar :: stackTail when next = Map.find openChar pairs -> loop stackTail tail
            | _ :: _ ->
                if withStack then
                    None
                else
                    Some [ next ]
            | [] -> if withStack then Some stack else None
        | _, _ :: checkTail -> loop stack checkTail
        | _, [] -> if withStack then Some stack else None

    loop [] (Seq.toList line)

let getOffendersNoStack = getOffenders false
let getOffendersWithStack = getOffenders true

let computeScore offender =
    match offender with
    | '(' -> 1
    | '[' -> 2
    | '{' -> 3
    | '<' -> 4
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "Invalid offender"

let flipOffenders offenders =
    offenders |> List.map (fun s -> pairs.[s])

let part1 =
    parseInput
    |> Array.choose getOffendersNoStack
    |> Array.map (fun s -> s.[0])
    |> Array.map computeScore
    |> Array.sum

let countMissing line =
    line
    |> List.fold (fun acc o -> (acc * 5L) + (computeScore o |> int64)) 0L

let part2 =
    parseInput
    |> Array.choose getOffendersWithStack
    |> Array.map countMissing
    |> Array.sort
    |> fun s -> Array.item (Array.length s / 2) s

printfn "%A" (part1, part2)
