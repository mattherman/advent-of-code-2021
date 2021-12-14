#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

let createBitGrid (values: string seq) =
    let stringToBitArray text =
        text |> Seq.map (string >> int) |> Seq.toArray

    values
    |> Seq.map stringToBitArray

let oxygenGeneratorBitCriteria column (grid: int[] seq) =
    grid
    |> Seq.groupBy (fun diagnosticBits -> diagnosticBits.[column])
    |> Seq.sortByDescending fst // Should favor the "1" bit result if equal
    |> Seq.maxBy (fun (_, matchingNumbers) -> matchingNumbers |> Seq.length)
    |> snd

let co2ScrubberBitCriteria column (grid: int[] seq) =
    grid
    |> Seq.groupBy (fun diagnosticBits -> diagnosticBits.[column])
    |> Seq.sortBy fst // Should favor the "0" bit result if equal
    |> Seq.minBy (fun (_, matchingNumbers) -> matchingNumbers |> Seq.length)
    |> snd

let binaryStringToDecimal (str: string) =
    $"0b{str}" |> int

let findRatingByBitCriteria (bitCriteria: int -> int[] seq -> int[] seq) (grid: int[] seq) =
    let rec findRating numbers column =
        let matches = numbers |> bitCriteria column |> Seq.toList
        match matches with
        | [singleMatch] ->
            singleMatch
            |> Array.map string
            |> String.concat ""
            |> binaryStringToDecimal
        | remainingNumbers -> findRating remainingNumbers (column + 1)

    findRating (grid |> Seq.toList) 0

let calculateOxygenGeneratorRating (grid: int[] seq) =
    grid |> findRatingByBitCriteria oxygenGeneratorBitCriteria

let calculateCO2ScrubberRating (grid: int[] seq) =
    grid |> findRatingByBitCriteria co2ScrubberBitCriteria

let solve () =
    let grid = readInput "input.txt" |> createBitGrid
    let oxygenGeneratorRating = calculateOxygenGeneratorRating grid
    let co2ScrubberRating = calculateCO2ScrubberRating grid
    oxygenGeneratorRating * co2ScrubberRating

let solution = solve ()
printfn "%d" solution
