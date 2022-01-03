#!/usr/bin/dotnet fsi

#load "./Exercise1.fsx"

open Exercise1

let solution = solve "input.txt" 256
printfn "%d" solution