#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

// 2 4 6 8 10 12 14 16 18 20
// Max = 20
// Length = 10
// Num Buckets = 4
// 20 / 4 = 5
// [(0, [2;4]), (1, [6;8]), (2, [10;12;14]), (3, [16;18]), (4, [20])]
module List =
    let bucket numBuckets list =
        if numBuckets <= 1 then
            [(0, list)]
        else
            let max = List.max list
            let increment = max / (numBuckets - 1)
            list |> List.groupBy (fun n -> n / increment)

let distance a b =
    abs (a - b)

let findMostFuelEfficientValue testValues allValues =
    testValues
    |> List.map (fun testValue ->
        let totalFuel = allValues |> List.sumBy (distance testValue)
        (testValue, totalFuel))
    |> List.minBy snd

let solve file =
    let input = readInput file
    let values =
        input
        |> Seq.head
        |> String.split ","
        |> List.map int
    let buckets =
        values
        |> List.bucket 4
    let largestBucket = buckets |> List.maxBy (snd >> List.length) |> snd
    findMostFuelEfficientValue largestBucket values |> fst

let solution = solve "input.txt"
printfn "%d" solution