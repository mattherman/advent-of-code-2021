#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

// 0:  [3]                             -> 1
// 1:  [2]                             -> 1
// 2:  [1]                             -> 1
// 3:  [0]                             -> 1
// 4:  [6 [8]]                         -> 2
// 5:  [5 [7]]                         -> 2
// 6:  [4 [6]]                         -> 2
// 7:  [3 [5]]                         -> 2
// 8:  [2 [4]]                         -> 2
// 9:  [1 [3]]                         -> 2
// 10: [0 [2]]                         -> 2
// 11: [6 [1]             [8]]         -> 3
// 12: [5 [0]             [7]]         -> 3
// 13: [4 [6 [8]]         [6]]         -> 4
// 14: [3 [5 [7]]         [5]]         -> 4
// 15: [2 [4 [6]]         [4]]         -> 4
// 16: [1 [3 [5]]         [3]]         -> 4
// 17: [0 [2 [4]]         [2]]         -> 4
// 18: [6 [1 [3]]         [1]     [8]] -> 5
// 19: [5 [0 [2]]         [0]     [7]] -> 5
// 20: [4 [6 [1]    [8]]  [6 [8]] [6]] -> 7
// 21: [3 [5 [0]    [7]]  [6 [7]] [5]] -> 7
// 22: [2 [4 [6 [8] [6]]] [5 [6]] [4]] -> 8

let rec simulate fish numDays : int =
    let spawnCycle = seq { fish .. 7 .. numDays } |> Seq.toList
    if spawnCycle.IsEmpty then
        let birthday = fish - 9
        if birthday <= numDays then
            1
        else
            0 // Do not count any fish that would have spawned after the cutoff
    else
        let numDescendants =
            spawnCycle
            |> Seq.map (fun newFish -> simulate (newFish + 9) numDays)
            |> Seq.sum
        1 + numDescendants

let solve file numDays =
    let fish =
        readInput file
        |> Seq.head
        |> String.split ","
        |> Seq.map int
    fish
    |> Seq.map (fun fish -> simulate (fish + 1) numDays)
    |> Seq.sum

let solution = solve "input.txt" 80
printfn "%d" solution