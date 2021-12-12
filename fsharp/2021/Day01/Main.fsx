let hasDepthIncrease pair = fst pair < snd pair

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Seq.map int

let part1 input =
    input
    |> Seq.pairwise
    |> Seq.filter hasDepthIncrease
    |> Seq.length

let part2 input =
    input
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> part1

printfn "%A" (part1 input, part2 input)
