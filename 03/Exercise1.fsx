#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

let createBitGrid (values: string seq) =
    let stringToBitArray text =
        text |> Seq.map (string >> int) |> Seq.toArray

    values
    |> Seq.map stringToBitArray

let countBitsInColumn column (grid: int[] seq) =
    grid
    |> Seq.countBy (fun diagnosticBits -> diagnosticBits.[column])

let mostCommonBit (grid: int[] seq) column =
    grid
    |> countBitsInColumn column
    |> Seq.maxBy (fun (_, count) -> count)
    |> fst

let leastCommonBit (grid: int[] seq) column =
    grid
    |> countBitsInColumn column
    |> Seq.minBy (fun (_, count) -> count)
    |> fst

let binaryStringToDecimal (str: string) =
    $"0b{str}" |> int

let calculateGamma (grid: int[] seq) =
    seq { 0 .. 11 }
    |> Seq.map (mostCommonBit grid >> string)
    |> String.concat ""
    |> binaryStringToDecimal

let calculateEpsilon (grid: int[] seq) =
    seq { 0 .. 11 }
    |> Seq.map (leastCommonBit grid >> string)
    |> String.concat ""
    |> binaryStringToDecimal

let solve () =
    let grid = readInput "input.txt" |> createBitGrid

    let gamma = calculateGamma grid
    let epsilon = calculateEpsilon grid

    gamma * epsilon

let solution = solve ()
printfn "%d" solution