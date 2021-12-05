module Day4

open System.IO
open System

let winningArray (array: option<int> array) =
    array |> Array.choose id |> Array.isEmpty

let checkWinner (board: option<int> [,]) =
    let rec loop idx isWinner =
        if isWinner then
            true
        elif idx = 5 then
            false
        else
            let row = board.[idx, *]
            let col = board.[*, idx]
            loop (idx + 1) (winningArray row || winningArray col)

    loop 0 false

let markNumberOnBoard number board =
    board
    |> Array2D.map (fun option ->
        match option with
        | Some n -> if n = number then None else Some n
        | None -> None)

let rec findWinners numbers boards winnersOutput : (int * option<int> [,]) list =
    match numbers with
    | [] -> winnersOutput
    | number :: numbersTail ->
        let markedBoards =
            List.map (markNumberOnBoard number) boards

        let winners = List.filter checkWinner markedBoards

        findWinners
            numbersTail
            (List.except winners markedBoards)
            (winnersOutput
             @ (winners |> List.map (fun board -> number, board)))

let boardSummer winningBoard =
    let (number, board) = winningBoard
    let numberSummer n = number * n

    board
    |> Seq.cast
    |> Seq.choose id
    |> Seq.fold (+) 0
    |> numberSummer

let playToWin numbers boards =
    findWinners numbers boards []
    |> List.item 0
    |> boardSummer

let playToLose numbers boards =
    findWinners numbers boards []
    |> List.last
    |> boardSummer

[<EntryPoint>]
let main argv =
    let (head :: tail) =
        File.ReadAllLines "input"
        |> Seq.filter (fun s -> not (String.IsNullOrEmpty s))
        |> Seq.toList

    let numbers =
        head
        |> (fun s -> s.Split "," |> Seq.map int)
        |> Seq.toList

    let boards =
        tail
        |> List.chunkBySize 5
        |> List.map (fun board ->
            List.map (fun (line: string) -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries)) board)
        |> Seq.map (fun board -> Array2D.init 5 5 (fun x y -> Some(board |> Seq.item x |> Seq.item y |> int)))
        |> Seq.toList

    let part1 = playToWin numbers boards
    let part2 = playToLose numbers boards
    printfn "%A %A" part1 part2
    0
