[<AutoOpen>]
module Day11

open System.IO

let parseInput =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> array2D
    |> Array2D.map (string >> int)

let checkBounds input (row, col) =
    (row >= 0 && row <= (Array2D.length1 input - 1))
    && (col >= 0 && col <= (Array2D.length2 input - 1))

let validNeighbours (input: int [,]) (row: int) (col: int) =
    [ (row - 1, col - 1)
      (row, col + 1)
      (row - 1, col + 1)
      (row - 1, col)
      (row + 1, col + 1)
      (row, col - 1)
      (row + 1, col - 1)
      (row + 1, col) ]
    |> List.filter (checkBounds input)
    |> List.filter (fun (row, col) -> input.[row, col] > 0)

let find2D pred (haystack: 'a [,]) =
    let rec loop row col =
        if col >= haystack.GetLength 1 then None
        elif row >= haystack.GetLength 0 then loop 0 (col + 1)
        elif pred haystack.[row, col] then Some(row, col)
        else loop (row + 1) col

    loop 0 0

let nextOctupus grid = grid |> find2D (fun n -> n > 9)

let resetGrid grid =
    grid
    |> Array2D.map (fun n -> if n = -1 then 0 else n)

let step grid =
    let rec stimulate count grid =
        match nextOctupus grid with
        | None -> (resetGrid grid, count)
        | Some (row, col) ->
            grid.[row, col] <- -1

            let nextGrid =
                validNeighbours grid row col
                |> List.fold
                    (fun (grid: int [,]) (row, col) ->
                        grid.[row, col] <- grid.[row, col] + 1
                        grid)
                    grid

            stimulate (count + 1) nextGrid

    grid |> Array2D.map ((+) 1) |> stimulate 0

let steps limit grid =
    let rec loop runs count grid =
        if limit runs grid then
            (count, runs)
        else
            let nextGrid, flashes = step grid
            loop (runs + 1) (count + flashes) nextGrid

    loop 0 0 grid

let part1 = steps (fun runs _ -> runs = 100) parseInput |> fst

let part2 = steps (fun _ grid -> grid |> Seq.cast<int> |> Seq.forall (fun n -> n = 0)) parseInput |> snd

printfn "%A" (part1, part2)
