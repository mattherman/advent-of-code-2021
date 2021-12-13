#!/usr/bin/dotnet fsi

#load "../Common.fsx"
open Common

let countIncreases (values: int[] list) =
    let rec loop (count, previous) list =
        match list with
        | [] -> count
        | window::remaining ->
            let previousWindowSum = Array.sum previous
            let currentWindowSum = Array.sum window
            if currentWindowSum > previousWindowSum then
                loop (count + 1, window) remaining
            else
                loop (count, window) remaining

    loop (0, List.head values) values

let solve () =
    readInput "input.txt"
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.toList
    |> countIncreases

let solution = solve ()
printfn "%d" solution