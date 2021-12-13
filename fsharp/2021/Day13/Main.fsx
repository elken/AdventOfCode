open System.IO

let parseInput =
    let lines =
        File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"

    let coordLines = lines |> Array.takeWhile ((<>) "")

    let foldLines =
        lines
        |> Array.skip (Array.length coordLines |> (+) 1)

    let coords =
        coordLines
        |> Array.map (fun s ->
            let tokens = s.Split(",")
            (tokens.[0] |> int, tokens.[1] |> int))
        |> Set.ofArray

    let folds =
        foldLines
        |> Array.map (fun (s: string) ->
            let tokens = s.Substring(11).Split("=")
            (tokens.[0], tokens.[1] |> int))

    (folds, coords)

let fold direction coords =
    Set.map
        (fun (x, y) ->
            match direction with
            | "x", n when n < x -> (n - (x - n), y)
            | "y", n when n < y -> (x, n - (y - n))
            | _ -> (x, y))
        coords

let output coords =
    let maxX = coords |> Seq.map fst |> Seq.max
    let maxY = coords |> Seq.map snd |> Seq.max

    [ for y in 0 .. maxY do
          [ for x in 0 .. maxX ->
                if coords |> Set.contains (x, y) then
                    "#"
                else
                    "." ] ]

let (folds, coords) = parseInput
let part1 = coords |> (fold folds.[0]) |> Set.count

let part2 =
    folds
    |> Seq.fold (fun acc n -> fold n acc) coords
    |> output
    |> List.map (String.concat "")

printfn "%A" (part1, part2)
