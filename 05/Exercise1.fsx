#!/usr/bin/dotnet fsi

#load "../Common.fsx"

open Common

type Point = int * int

type VentLine = {
    Start: Point
    End: Point
}

let either (condA: 'a -> bool) (condB: 'a -> bool) (value: 'a) =
    (condA value) || (condB value)

let isHorizontal (line: VentLine) =
    (line.Start |> snd) = (line.End |> snd)

let isVertical (line: VentLine) =
    (line.Start |> fst) = (line.End |> fst)

let sortPair (first, second) =
    if first > second then
        (second, first)
    else
        (first, second)

let getAllPoints (line: VentLine) =
    let (x1, y1) = line.Start
    let (x2, y2) = line.End
    let (lowX, highX) = sortPair (x1, x2)
    let (lowY, highY) = sortPair (y1, y2)
    seq {
        for x in lowX .. highX do
            for y in lowY .. highY do
                yield (x, y)
    }

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
    let ventLines =
        parseLines (input |> Seq.toList)
        |> List.filter (either isHorizontal isVertical)
    let allVentLinePoints = ventLines |> List.map getAllPoints |> Seq.concat
    let ventLinePointCounts = allVentLinePoints |> Seq.countBy id
    let dangerousPoints = ventLinePointCounts |> Seq.filter (fun (_, count) -> count >= 2)
    dangerousPoints |> Seq.length

let solution = solve ("input.txt")
printfn "%d" solution