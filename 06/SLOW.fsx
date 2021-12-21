#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

type Lanternfish = {
    Timer: int
}

let toLanternfish initialTimer =
    { Timer = initialTimer }

let ageFish fish =
    if fish.Timer = 0 then
        [{ Timer = 6 }; { Timer = 8 }]
    else
        [{ Timer = fish.Timer - 1}]

let simulate fish numDays =
    let rec tick fish day numDays =
        printfn "%d" day
        if day = numDays then
            fish
        else
            let updatedFish =
                fish
                |> List.fold (fun acc elem -> acc @ (ageFish elem)) []
            tick updatedFish (day + 1) numDays

    tick fish 0 numDays

let parseInput input =
    input
    |> Seq.head
    |> String.split ","
    |> Seq.map (int >> toLanternfish)
    |> Seq.toList

let solve (file) =
    let input = readInput file
    let fish = parseInput input
    let result = simulate fish 80
    List.length result

let solution = solve ("input.txt")
printfn "%d" solution