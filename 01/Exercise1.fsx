#!/usr/bin/dotnet fsi

#load "../Common.fsx"
open Common

let countIncreases (values: int list) =
    let rec loop (count, previous) list =
        match list with
        | [] -> count
        | head::tail ->
            if head > previous then
                loop (count + 1, head) tail
            else
                loop (count, head) tail

    loop (0, List.head values) values

let solve () =
    readInput "input.txt"
    |> Seq.map int
    |> Seq.toList
    |> countIncreases

let solution = solve ()
printfn "%d" solution
