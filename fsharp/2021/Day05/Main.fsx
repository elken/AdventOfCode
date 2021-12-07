[<AutoOpen>]
module Day5

open System.IO

let lineToPoints line =
    let p1 = List.item 0 line
    let p2 = List.item 1 line
    (List.item 0 p1, List.item 0 p2, List.item 1 p1, List.item 1 p2)

let isCardinal line =
    let (x1, x2, y1, y2) = lineToPoints line
    x1 = x2 || y1 = y2

let maxPoints points = (List.min points, List.max points)

let makePoints line =
    let (x1, x2, y1, y2) = lineToPoints line
    let (xMin, xMax) = maxPoints [ x1; x2 ]
    let (yMin, yMax) = maxPoints [ y1; y2 ]

    if isCardinal line then
        [ for x in xMin .. xMax do
              for y in yMin .. yMax -> (x, y) ]
    else
        let xs = if x1 < x2 then 1 else -1
        let ys = if y1 < y2 then 1 else -1

        [ for gap in 0 .. (xMax - xMin) -> (x1 + (xs * gap), y1 + (ys * gap)) ]

let hasIntersection point =
    let (_coords, count) = point
    count > 1

let sumOverlaps points =
    points
    |> List.collect makePoints
    |> Seq.countBy id
    |> Seq.filter hasIntersection
    |> Seq.length

let overlapCardinals input =
    input |> List.filter isCardinal |> sumOverlaps

let overlapLines input = input |> sumOverlaps

let parseInput =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Seq.map (fun line ->
        line.Split("->")
        |> Seq.map (fun point -> point.Split(",") |> Seq.map int |> Seq.toList)
        |> Seq.toList)
    |> Seq.toList

let input = parseInput
let part1 = overlapCardinals input
let part2 = overlapLines input
printfn "%A" (part1, part2)
