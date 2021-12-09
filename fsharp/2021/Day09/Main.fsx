[<AutoOpen>]
module Day9

open System.IO

let checkBounds input (row, col) =
    (row >= 0 && row <= (Array2D.length1 input - 1))
    && (col >= 0 && col <= (Array2D.length2 input - 1))

let neighbours (input: int [,]) (row: int) (col: int) =
    [ (row - 1, col)
      (row + 1, col)
      (row, col - 1)
      (row, col + 1) ]
    |> List.filter (checkBounds input)
    |> List.map (fun (x, y) -> input.[x, y])

let isLowPoint (n, _, neighbours) = n < (Seq.min neighbours)

let parseInput =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Array.map (fun s -> Seq.map (string >> int) s |> Array.ofSeq)
    |> array2D

let rec basinSize (input: int [,]) row col =
    if row < 0
       || row >= Array2D.length1 input
       || col < 0
       || col >= Array2D.length2 input
       || input.[row, col] = 9 then
        0
    else
        input.[row, col] <- 9

        1
        + (basinSize input (row - 1) col)
        + (basinSize input (row + 1) col)
        + (basinSize input row (col - 1))
        + (basinSize input row (col + 1))

let fst (a, _, _) = a

let part1 =
    parseInput
    |> Array2D.mapi (fun row col n -> n, (row, col), neighbours parseInput row col)
    |> Seq.cast<int * (int * int) * int list>
    |> Seq.filter isLowPoint
    |> Seq.map (fst >> (+) 1)
    |> Seq.sum

let product list =
    let rec loop list acc =
        match list with
        | head :: tail -> loop tail (acc * head)
        | [] -> acc

    loop list 1

let basins input =
    let w = Array2D.length1 input
    let h = Array2D.length2 input

    [ for row in 0 .. (w - 1) do
          for col in 0 .. (h - 1) ->
              let n = input.[row, col]

              if n = 9 then
                  0
              else
                  basinSize input row col ]
    |> List.filter (fun n -> not (n = 0))
    |> List.sort
    |> List.rev
    |> List.take 3
    |> product

let part2 = basins parseInput

printfn "%A %A" part1 part2
