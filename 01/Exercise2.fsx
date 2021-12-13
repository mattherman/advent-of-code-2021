#!/usr/bin/dotnet fsi

#load "../Common.fsx"
open Common

let countIncreases (values: int[] list) =
    let rec loop (count, previousWindow) list =
        match list with
        | [] -> count
        | currentWindow::remaining ->
            let previousWindowSum = Array.sum previousWindow
            let currentWindowSum = Array.sum currentWindow
            if currentWindowSum > previousWindowSum then
                loop (count + 1, currentWindow) remaining
            else
                loop (count, currentWindow) remaining

    loop (0, List.head values) values

let solve () =
    readInput "input.txt"
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.toList
    |> countIncreases

let solution = solve ()
printfn "%d" solution