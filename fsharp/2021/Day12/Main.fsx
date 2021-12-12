open System
open System.IO

let isSmallCave (cave: string) = Char.IsLower cave.[0]

let mapAdd key value map =
    match Map.tryFind key map with
    | Some c -> value :: c
    | None -> [ value ]

let addNodeToMap key value map = Map.add key (mapAdd key value map) map

let merge (m1: Map<_, _>) (m2: Map<_, _>) =
    let add m k v1 =
        let v =
            match Map.tryFind k m with
            | Some v2 -> v2 @ v1 |> Set.ofList |> Set.toList
            | None -> v1

        Map.add k v m

    Map.fold add m1 m2

let parseLine (map: Map<string, string list>) (line: string) =
    match line.Split("-") with
    | [| lhs; rhs |] -> merge (addNodeToMap lhs rhs map) (addNodeToMap rhs lhs map)
    | _ -> failwith "Invalid line"

let findPaths canBacktrack (paths: Map<string, string list>) =
    let rec loop node visited cave =
        let v =
            if node = "end" then
                1
            elif node = "start" && Set.count visited > 0 then
                0
            elif isSmallCave node && Set.contains node visited then
                if canBacktrack then
                    if cave = "" then
                        paths.[node]
                        |> List.fold (fun acc n -> acc + (loop n (Set.add node visited) node)) 0
                    else
                        0
                else
                    0
            else
                -1

        if v = -1 then
            paths.[node]
            |> List.fold (fun acc n -> acc + (loop n (Set.add node visited) cave)) 0
        else
            v

    loop "start" Set.empty ""

let parseInput =
    File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input"
    |> Array.fold parseLine Map.empty

let part1 = parseInput |> findPaths false

let part2 = parseInput |> findPaths true

printfn "%A" (part1, part2)
