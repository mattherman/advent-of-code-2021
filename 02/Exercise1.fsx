#!/usr/bin/dotnet fsi

#load "../Common.fsx"
open Common

module Seq =
    let chooseResult (chooser: 'a -> Result<'b, 'c>) sequence =
        sequence
        |> Seq.choose (fun result ->
            match chooser result with
            | Ok value -> Some value
            | Error _ -> None)

type SubmarinePosition = {
    Horizontal: int;
    Depth: int;
}

type Command =
    | Forward of int
    | Down of int
    | Up of int

let applyCommand (command: Command) (position: SubmarinePosition) =
    match command with
    | Forward distance ->
        { position with Horizontal = position.Horizontal + distance }
    | Down distance ->
        { position with Depth = position.Depth + distance }
    | Up distance ->
        { position with Depth = position.Depth - distance }

let rec followCourse (position: SubmarinePosition) (commands: Command list) =
    match commands with
    | [] -> position
    | current::remaining ->
        let newPosition = position |> applyCommand current
        followCourse newPosition remaining

let parseCommand (text: string) =
    let parts = text.Split([| ' ' |])
    match parts with
    | [| name; value |] ->
        match name with
        | "forward" -> Ok (Forward (int value))
        | "down"    -> Ok (Down (int value))
        | "up"      -> Ok (Up (int value))
        | _         -> Error "Command name must match 'forward', 'down', or 'up'"
    | [||] -> Error "Empty line"
    | _ -> Error "Expected format to match '<name> <value>"

let solve () =
    let commands =
        readInput "input.txt"
        |> Seq.chooseResult parseCommand 
        |> Seq.toList
    let finalPosition = followCourse { Horizontal = 0; Depth = 0; } commands
    finalPosition.Horizontal * finalPosition.Depth

let solution = solve ()
printfn "%d" solution