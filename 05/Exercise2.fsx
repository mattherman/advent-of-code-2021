#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

type Point = int * int

type VentLine = {
    Start: Point
    End: Point
}

let isDiagonal (line: VentLine) =
    let (x1, y1) = line.Start
    let (x2, y2) = line.End
    x1 <> x2 && y1 <> y2

let sortPair (first, second) =
    if first > second then
        (second, first)
    else
        (first, second)

let getOrderedSequence (first, second) =
    let (low, high) = sortPair (first, second)
    let sequence = seq { low .. high }
    if low <> first then
        sequence |> Seq.rev
    else
        sequence

// (1,0) -> (4,3) = [(1,0),(2,1),(3,2),(4,3)]
//
// . * . . . . .
// . . * . . . .
// . . . * . . .
// . . . . * . .
//
// x1 .. x2 = 1 .. 4 = [1;2;3;4]
// y1 .. y2 = 0 .. 3 = [0;1;2;3]
// => Zip [1;2;3;4] [0;1;2;3]
//
// (0,3) -> (3,0) = [(0,3),(1,2),(2,1),(3,0)]
//
// . . . * . . .
// . . * . . . .
// . * . . . . .
// * . . . . . .
//
// x1 .. x2 = 0 .. 3 = [0;1;2;3]
// y2 .. y1 = 0 .. 3 = [0;1;2;3]
// For non-ascending directions, sort to get list and then reverse
// => Zip [0;1;2;3] [3;2;1;0]
let getAllDiagonalPoints (line: VentLine) =
    let (x1, y1) = line.Start
    let (x2, y2) = line.End
    let xPoints = getOrderedSequence (x1, x2)
    let yPoints = getOrderedSequence (y1, y2)
    Seq.zip xPoints yPoints

let getAllOrthogonalPoints (line: VentLine) =
    let (x1, y1) = line.Start
    let (x2, y2) = line.End
    let (lowX, highX) = sortPair (x1, x2)
    let (lowY, highY) = sortPair (y1, y2)
    seq {
        for x in lowX .. highX do
            for y in lowY .. highY do
                yield (x, y)
    }

let getAllPoints (line: VentLine) =
    if isDiagonal line then
        getAllDiagonalPoints line
    else
        getAllOrthogonalPoints line

let parsePoint pair : Point =
    match pair |> String.split "," with
    | [ x; y ] -> (int x, int y)
    | _ -> (0, 0)

let parseLine line =
    let lineParts = line |> String.split " -> "
    let lineStart = parsePoint lineParts.[0]
    let lineEnd = parsePoint lineParts.[1]
    { Start = lineStart; End = lineEnd }

let parseLines lines =
    lines |> List.map parseLine

let solve (inputFile) =
    let input = readInput inputFile
    let ventLines = parseLines (input |> Seq.toList)
    let allVentLinePoints = ventLines |> List.map getAllPoints |> Seq.concat
    let ventLinePointCounts = allVentLinePoints |> Seq.countBy id
    let dangerousPoints = ventLinePointCounts |> Seq.filter (fun (_, count) -> count >= 2)
    dangerousPoints |> Seq.length

let solution = solve ("input.txt")
printfn "%d" solution